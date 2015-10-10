//! adder crate provides functions to add numbers
//!
//! # Examples
//!
//! ```
//! assert_eq!(4, adder::add_two(2));
//! ```

/// This function adds two to its argument.
///
/// # Examples
///
/// ```
/// use adder::add_two;
///
/// assert_eq!(4, add_two(2));
/// ``` 
pub fn add_two(a: i32) -> i32 {
    a + 2
}

#[test]
#[should_panic(expected = "assertion failed")]
fn it_works() {
    assert_eq!("Hello", "world");

}



#[cfg(test)]
mod tests{
use super::*;

    #[test]
    fn it_works_too() {
    assert_eq!(4, add_two(2));
    }
}

