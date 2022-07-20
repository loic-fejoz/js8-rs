use async_io::Timer;
use std::cmp;
use std::ptr;
use std::time::SystemTime;
use std::time::Instant;

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
