fn main() {
    let mut prime =0;
    let mut x=1;
    while prime<10001 {
        x=x+1;
        let mut isprime = true;
        for i in 2..((x as f64).sqrt() as i64)+1 {
            if x%i==0 {
                isprime=false;
            }
        }
        if isprime==true {
            println!("{}",x);
            prime=prime +1
        }

    }
}

