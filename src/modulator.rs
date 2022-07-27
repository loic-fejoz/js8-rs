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

#[derive(Debug, Copy, Clone)]
pub struct ContinuousPhaseModulator {
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