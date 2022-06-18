extern crate strum_macros;

#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;


pub mod pack;
pub mod js8frame;
pub mod compound;