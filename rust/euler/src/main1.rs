fn main() {
    let a = (1..1000).filter(|&x| (x % 3 ==0 || x % 5 == 0)).fold(0, |sum, x| sum + x);
    println!("Jaja {}", a);
}
