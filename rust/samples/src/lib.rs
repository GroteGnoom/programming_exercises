//! Simple code snippets with tests
//!
//! # Examples
//!
//! ```
//! assert_eq!(4, samples::add_two(2));
//! ```

//extern crate num;
//use std::num::{One, Zero};
//use num::integer::Integer;
//use num::traits::FromPrimitive;
//use std::cmp::Eq;

/// This function adds two to its argument. Part of the rust book.
///
/// # Examples
///
/// ```
/// use samples::add_two;
///
/// assert_eq!(4, add_two(2));
/// ```
pub fn add_two(a: i32) -> i32 {
    a + 2
}

/// factorial2: takes takes a u32 and returns its factorial
///
/// # Examples
///
/// ```
/// use samples::factorial2;
///
/// assert_eq!(120, factorial2(5));
/// ```
pub fn factorial2(x: u32) -> u32 {
    let mut total = 1u32;
    for i in 1..(x+1) {
        total *= i
    }
    total
}


/// factorial: takes an unsigned integer and returns its factorial
/// does not check for negative numbers
///
/// # Examples
///
/// ```
/// use samples::factorial;
///
/// assert_eq!(120, factorial(5));
/// assert_eq!(1, factorial(0));
/// ```
pub fn factorial(x: u64) -> u64 {
    (1..x+1).fold(1, |p, x| p*x)
}





#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(4, add_two(2));
    }
}



