fn main() {
    let n = 600851475143;
    let mut m = n;
    let mut div = 2;
    while div<(n as f64).sqrt() as i64 {
        let mut new = true;
        while new==true {
            if m % div == 0 {
                m = m / div;
                println!("Jaja {}", div);
            }
            else {
                new = false;
            }
        }
        div = div + 1;
    }       
}
