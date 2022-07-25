use async_io::Timer;
use std::cmp;
use std::ptr;

use futuresdr::anyhow::Result;
use futuresdr::runtime::Block;
use futuresdr::runtime::BlockMeta;
use futuresdr::runtime::BlockMetaBuilder;
use futuresdr::runtime::Kernel;
use futuresdr::runtime::MessageIo;
use futuresdr::runtime::MessageIoBuilder;
use futuresdr::runtime::StreamIo;
use futuresdr::runtime::StreamIoBuilder;
use futuresdr::runtime::WorkIo;

/// Release samples based on timing provided by the release pattern function.
///
/// # Inputs
///
/// `in`: Input
///
/// # Outputs
///
/// `out`: Output
///
/// # Usage
/// ```
/// use futuresdr::blocks::strobe;
/// use futuresdr::runtime::Flowgraph;
/// use num_complex::Complex;
///
/// let mut fg = Flowgraph::new();
///
/// let strobe = fg.add_block(Strobe::<Complex<f32>>::new(1_000_000.0));
/// ```
#[cfg_attr(docsrs, doc(cfg(not(target_arch = "wasm32"))))]
pub struct Strobe<T>
where
    T: Send + 'static,
{
    previous_release_count: usize,
    pattern: Box<dyn FnMut(usize, usize) -> (usize, Option<Timer>, Option<Timer>) + Send + 'static>,
    _type: std::marker::PhantomData<T>,
}

impl<T: Send + 'static> Strobe<T> {
    pub fn new(f: impl FnMut(usize, usize) -> (usize, Option<Timer>, Option<Timer>) + Send + 'static) -> Block {
        Block::new(
            BlockMetaBuilder::new("Strobe").build(),
            StreamIoBuilder::new()
                .add_input("in", std::mem::size_of::<T>())
                .add_output("out", std::mem::size_of::<T>())
                .build(),
            MessageIoBuilder::<Self>::new().build(),
            Strobe::<T> {
                previous_release_count: 0,
                pattern: Box::new(f),
                _type: std::marker::PhantomData,
            },
        )
    }
}

#[async_trait]
impl<T: Send + 'static> Kernel for Strobe<T> {
    async fn work(
        &mut self,
        io: &mut WorkIo,
        sio: &mut StreamIo,
        _mio: &mut MessageIo<Self>,
        _meta: &mut BlockMeta,
    ) -> Result<()> {
        if io.block_on.is_some() {
            //io.call_again = true;
            return Ok(());
        }

        let i = sio.input(0).slice::<T>();
        let o = sio.output(0).slice::<T>();

        let mut m = cmp::min(i.len(), o.len());

        let (target_items_count, immediate_timer, next_timer) = (self.pattern)(self.previous_release_count, m);
        
        if let Some(timer) = immediate_timer {
            async_io::block_on(timer);
        }
        m = cmp::min(m, target_items_count) as usize;
        if m != 0 {
            unsafe {
                ptr::copy_nonoverlapping(i.as_ptr(), o.as_mut_ptr(), m);
            }

            self.previous_release_count = m;
            sio.input(0).consume(m);
            sio.output(0).produce(m);
        }

        if sio.input(0).finished() && i.len() == m {
            io.finished = true;
        }

        if let Some(timer) = next_timer {
            io.block_on(async {
                timer.await;
            });
        }

        Ok(())
    }

    async fn init(
        &mut self,
        _sio: &mut StreamIo,
        _mio: &mut MessageIo<Self>,
        _meta: &mut BlockMeta,
    ) -> Result<()> {
        self.previous_release_count = 0;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::Strobe;
    use async_io::Timer;
    use std::time::Duration;
    use futuresdr::blocks::FiniteSource;
    use futuresdr::runtime::Flowgraph;
    use futuresdr::runtime::Runtime;
    use futuresdr::runtime::buffer::circular::Circular;
    use rand::distributions::{Normal, Distribution};
    use std::time::SystemTime;
    use std::time::Instant;


    #[test]
    fn strobe_every_1s() {
        let mut fg = Flowgraph::new();

        // Qv+FqnSNwzu0 + bit 3
        const VALUES: [u8; 5] = [1, 2, 3, 4, 5];
        let mut src_iter = VALUES.iter();
        let src = FiniteSource::<&u8>::new(move || src_iter.next());
        let src = fg.add_block(src);

        // Release 1 every 1s
        let arrival_shaper = Strobe::<&u8>::new(move |_previous_count, _current_available| {
            let timer = Timer::after(Duration::from_secs(1));
            (1, None, Some(timer))
        });
        let arrival_shaper = fg.add_block(arrival_shaper);

        let mut starting_date = std::time::Instant::now();
        let apply = futuresdr::blocks::Apply::<&u8,f32>::new(move |v| {
            if **v == 1 {
                starting_date = std::time::Instant::now();
            }
            (std::time::Instant::now() - starting_date).as_secs_f32()
        });
        let apply = fg.add_block(apply);

        //let snk = NullSink::<f32>::new();
        let snk = futuresdr::blocks::ConsoleSink::<f32>::new("\n");
        let snk = fg.add_block(snk);

        fg.connect_stream(src, "out", arrival_shaper, "in").expect("");
        fg.connect_stream(arrival_shaper, "out", apply, "in").expect("");
        fg.connect_stream(apply, "out", snk, "in").expect("");

        Runtime::new().run(fg).expect("");

    }

    #[test]
    fn strobe_interarrival_normal2s() {
        let mut fg = Flowgraph::new();

        // Qv+FqnSNwzu0 + bit 3
        const VALUES: [u8; 6] = [1, 2, 3, 4, 5, 6];
        let mut src_iter = VALUES.iter();
        let src = FiniteSource::<&u8>::new(move || src_iter.next());
        let src = fg.add_block(src);

        // mean 2s, standard deviation 3
        let normal = Normal::new(2_000.0, 3.0);
        // Release up to 2 samples every 2s with standard deviation of 3 between 2
        let arrival_shaper = Strobe::<&u8>::new(move |_previous_count, _current_available| {
            let v = normal.sample(&mut rand::thread_rng());
            let timer = Timer::after(Duration::from_millis(v as u64));
            (2, None, Some(timer))
        });
        let arrival_shaper = fg.add_block(arrival_shaper);

        let mut starting_date = std::time::Instant::now();
        let apply = futuresdr::blocks::Apply::<&u8,(u8, f32)>::new(move |v| {
            if **v == 1 {
                starting_date = std::time::Instant::now();
            }
            (**v, (std::time::Instant::now() - starting_date).as_secs_f32())
        });
        let apply = fg.add_block(apply);

        //let snk = NullSink::<f32>::new();
        let snk = futuresdr::blocks::ConsoleSink::<(u8, f32)>::new("\n");
        let snk = fg.add_block(snk);

        fg.connect_stream(src, "out", arrival_shaper, "in").expect("");
        fg.connect_stream(arrival_shaper, "out", apply, "in").expect("");
        fg.connect_stream(apply, "out", snk, "in").expect("");

        Runtime::new().run(fg).expect("");

    }


    #[test]
    fn strobe_every_2s_wallclock() {
        let mut fg = Flowgraph::new();

        // Qv+FqnSNwzu0 + bit 3
        const VALUES: [u8; 6] = [1, 2, 3, 4, 5, 6];
        let mut src_iter = VALUES.iter();
        let src = FiniteSource::<&u8>::new(move || src_iter.next());
        let src = fg.add_block(src);

        // Release 1 sample every 2s align with wallclock
        const PERIOD: Duration = Duration::from_secs(2);
        let arrival_shaper = Strobe::<&u8>::new(move |_previous_count, current_available| {
            let wallclock = SystemTime::now();
            let now = Instant::now();
            let waiting_time = Duration::from_millis((PERIOD.as_millis() - wallclock.duration_since(SystemTime::UNIX_EPOCH).expect("").as_millis() % PERIOD.as_millis()) as u64);
            let timer = Timer::at(now + waiting_time - Duration::from_millis(500) /* security so as to be on time */);
            if current_available < 1 {
                return (0, None, Some(timer));
            } else {
                let next_next_timer = Timer::at(Instant::now() + waiting_time + PERIOD);
                if waiting_time < Duration::from_millis(500) {
                    // Close to deadline, just actively wait now
                    return (1, Some(timer), Some(next_next_timer));
                } else if waiting_time > PERIOD - Duration::from_millis(300) {
                    // We are not that late. Go NOW!
                    return (1, None, Some(timer));
                } else {
                    // Nope, just wait the next release date
                    return (0, Some(timer), Some(next_next_timer));
                }
            }
        });
        let arrival_shaper = fg.add_block(arrival_shaper);

        let apply = futuresdr::blocks::Apply::<&u8,(u8, f32)>::new(move |v| {
            (**v, SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).expect("").as_secs_f32())
        });
        let apply = fg.add_block(apply);

        //let snk = NullSink::<f32>::new();
        let snk = futuresdr::blocks::ConsoleSink::<(u8, f32)>::new("");
        let snk = fg.add_block(snk);

        let sample_size = std::mem::size_of::<(u8, f32)>();
        fg.connect_stream(
            src, "out", 
            arrival_shaper,"in").expect("");
        fg.connect_stream_with_type(
            arrival_shaper, "out",
            apply, "in",
            Circular::with_size(sample_size)).expect("");
        fg.connect_stream_with_type(
            apply, "out",
            snk, "in",
            Circular::with_size(sample_size)).expect("");

        Runtime::new().run(fg).expect("");

    }

    // TODO: illustrate throttle
}