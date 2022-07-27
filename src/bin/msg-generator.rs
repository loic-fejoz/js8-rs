#[macro_use]
pub extern crate async_trait;

use async_io::Timer;
use futuresdr::log::debug;
use js8::pack::JS8Protocol;
use std::time::{Duration, SystemTime, Instant};
use std::str::FromStr;

use futuresdr::anyhow::Result;
use futuresdr::blocks::ApplyIntoIter;
use futuresdr::blocks::audio::AudioSink;
use futuresdr::runtime::Flowgraph;
use futuresdr::runtime::Runtime;
use futuresdr::blocks::FiniteSource;
use futuresdr::num_complex::Complex;

use js8::js8frame::{Command, Frame};
use js8::strobe::Strobe;
use js8::modulator::ContinuousPhaseModulator;
use js8::compound::Compound;

fn main() -> Result<()> {
    let mut fg = Flowgraph::new();

    const SAMPLE_RATE: u32 = 48_000;
    const BASE_FREQ: f32 = 1330.0;
    const TONE_SPACING: f32 = 10.0;
    const PERIOD_MS: u128 = 10_000;

    let f = Frame::FrameDirectedMessage {
        from: Some(Compound::from_str("DR4CNK").expect("valid callsign")),
        to: Some(Compound::from_str("KN4CRD").expect("valid callsign")),
        cmd: Some(Command::AgainQuestion),
        num: None,
    };

    let f = JS8Protocol::pack_directed_frame(f).expect("");
    let tones = JS8Protocol::genjs8(
        f,
        js8::js8frame::TransmissionType::JS8CallUnique,
        js8::pack::CostasTones::Config2);

    let mut v = Vec::<u8>::new();
    v.extend(tones);
    let mut v = v.into_iter();

    let mut src = FiniteSource::<u8>::new(move || {
        v.next()
    });
    src.set_instance_name("Tones generator");
    let src = fg.add_block(src);

    // Release everything every 10s synchronized with wall clock
    // If we are less than 300ms late then still release.
    // If we are less than 500ms ahead of the release date, then do an active wait.
    let mut arrival_pattern_shaper = Strobe::<u8>::new(move |previous_count, current_available| {
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
    let tone_converter = ApplyIntoIter::new(move |a_tone: &u8| {
        fsk.emit(
            BASE_FREQ + (*a_tone as f32) * TONE_SPACING,
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
