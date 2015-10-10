fn main() {
    for i in (1..101) {
        let sum = (1..i+1).fold(0, |sum, x| sum +x);
        let sqsum=sum*sum;
        let sq=(1..i+1).map(|x| x*x);
        let sumsq=sq.fold(0, |sum,x| sum + x);
        println!("{}", sqsum-sumsq);
    }
}

