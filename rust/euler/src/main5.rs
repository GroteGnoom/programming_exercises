fn main() {
    let mut x = 0;
    loop {
        let mut div=true;
        x=x+1;
        for i in 1..21 {
            if x % i != 0 {
                div =false;
            };
        }
        if div == true {
            println!("{}",x);
            break;
        }
    }


}
//2*3*2*5*7*2*3*11*13*2*17*19

