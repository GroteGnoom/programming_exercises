fn main() {
    let mut fibnm1= 1;
    let mut fibn = 1;
    let mut sum = 0;
    while fibn<4000001{
        let temp = fibn;
        fibn = fibn+fibnm1;
        fibnm1 = temp;
        if fibn % 2 == 0 {sum = sum + fibn;}
    };
    println!("Jaja {}", sum);
}
