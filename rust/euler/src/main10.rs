fn main() {
    let mut sum =0;
    let mut x=1;
    for x in 2..2000000 {
        let mut isprime = true;
        for i in 2..((x as f64).sqrt() as i64)+1 {
            if x%i==0 {
                isprime=false;
            }
        }
        if isprime==true {
            println!("{}",x);
            sum=sum+x;
        }

    }
      
    println!("{}",sum);
}

