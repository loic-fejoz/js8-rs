#[macro_use]
pub extern crate async_trait;

use async_io::Timer;
use futuresdr::log::debug;
use std::time::{Duration, SystemTime, Instant};

use futuresdr::anyhow::Result;
use futuresdr::blocks::ApplyIntoIter;
use futuresdr::blocks::audio::AudioSink;
use futuresdr::runtime::Flowgraph;
use futuresdr::runtime::Runtime;
use futuresdr::blocks::FiniteSource;
use futuresdr::num_complex::Complex;

mod strobe;
use strobe::Strobe;

#[derive(Debug, Copy, Clone)]
struct ContinuousPhaseModulator {
    iq: Complex::<f32>,
    fwt0: f32,
    i_samples: usize,
    n_samples: usize,
}

impl ContinuousPhaseModulator {
    pub fn new(frequency: f32, sampling_rate: u32, n_samples: usize) -> ContinuousPhaseModulator {
        ContinuousPhaseModulator{
            iq: Complex::<f32>::new(1.0, 0.0),
            fwt0: 2.0 * std::f32::consts::PI * frequency / (sampling_rate as f32),
            i_samples: 0,
            n_samples
        }
    }

    pub fn emit(self, frequency: f32, sampling_rate: u32, n_samples: usize) -> ContinuousPhaseModulator {
        ContinuousPhaseModulator{
            iq: self.iq,
            fwt0: 2.0 * std::f32::consts::PI * frequency / (sampling_rate as f32),
            i_samples: 0,
            n_samples
        }
    }
}

impl Iterator for ContinuousPhaseModulator {
    type Item =  Complex::<f32>;

    fn next(&mut self) -> Option<Self::Item> {
        self.i_samples += 1;

        if self.i_samples < self.n_samples {
            let lo_v = Complex::<f32>::new( 0.0, (self.i_samples as f32) * self.fwt0).exp();
            Some(lo_v)
        } else {
            None
        }
    }
}

fn main() -> Result<()> {
    let mut fg = Flowgraph::new();

    const SAMPLE_RATE: u32 = 48_000;
    const BASE_FREQ: f32 = 1330.0;
    const TONE_SPACING: f32 = 10.0;
    const PERIOD_MS: u128 = 10_000;
    // Qv+FqnSNwzu0 + bit 3
    const TONES: [u8; 79] = [0, 6, 2, 3, 5, 4, 1, 2, 2, 6, 0, 3, 6, 6, 5, 4, 0, 2, 1, 6, 0, 7, 7, 0, 4, 1, 1, 0, 1, 2, 5, 4, 3, 3, 6, 0, 1, 5, 0, 2, 3, 6, 4, 3, 2, 7, 1, 7, 7, 1, 7, 6, 4, 6, 1, 3, 4, 2, 7, 7, 2, 7, 5, 7, 0, 0, 0, 3, 1, 6, 7, 4, 2, 5, 0, 6, 4, 1, 3];
    let mut tones_iter = TONES.iter();
    let mut src = FiniteSource::<&u8>::new(move || tones_iter.next());
    src.set_instance_name("Tones generator");
    let src = fg.add_block(src);

    // Release everything every 10s synchronized with wall clock
    // If we are less than 300ms late then still release.
    // If we are less than 500ms ahead of the release date, then do an active wait.
    let mut arrival_pattern_shaper = Strobe::<&u8>::new(move |previous_count, current_available| {
        debug!("pattern: {} {}", previous_count, current_available);
        let wallclock = SystemTime::now();
        let waiting_time = PERIOD_MS - wallclock.duration_since(SystemTime::UNIX_EPOCH).expect("").as_millis() % PERIOD_MS;
        debug!("waiting_time: {}", waiting_time);
        let timer = Timer::at(Instant::now() + Duration::from_millis(waiting_time as u64));
        if current_available < 79 {
            return (0, None, Some(timer));
        } else {
            let next_next_timer = Timer::at(Instant::now() + Duration::from_millis((waiting_time + PERIOD_MS) as u64));
            if waiting_time < 500 {
                // Just wait now
                return (current_available, Some(timer), Some(next_next_timer));
            } else if waiting_time > PERIOD_MS - 300 {
                // We are not that late. Go NOW!
                return (current_available, None, Some(timer));
            } else {
                // Nope, just wait the next release date
                return (0, Some(timer), Some(next_next_timer));
            }
        }
        
    });
    arrival_pattern_shaper.set_instance_name("every 10s wall clock");
    let arrival_pattern_shaper = fg.add_block(arrival_pattern_shaper);
    
    let fsk = ContinuousPhaseModulator::new(0.0, SAMPLE_RATE, 0);
    let tone_converter = ApplyIntoIter::new(move |a_tone: &&u8| {
        fsk.emit(
            BASE_FREQ + (**a_tone as f32) * TONE_SPACING,
            SAMPLE_RATE,
            4*1200 as usize)
        .map(|v: Complex<f32>| v.re)
    });
    let tone_converter = fg.add_block(tone_converter);

    let snk = AudioSink::new(SAMPLE_RATE, 1);
    let snk = fg.add_block(snk);

    fg.connect_stream(src, "out", arrival_pattern_shaper, "in")?;
    fg.connect_stream(arrival_pattern_shaper, "out", tone_converter, "in")?;
    fg.connect_stream(tone_converter, "out", snk, "in")?;

    Runtime::new().run(fg)?;

    Ok(())
}
