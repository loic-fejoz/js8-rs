#[macro_use]
pub extern crate async_trait;

extern crate strum_macros;

#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;


pub mod pack;
pub mod js8frame;
pub mod compound;
pub mod modulator;
pub mod strobe;