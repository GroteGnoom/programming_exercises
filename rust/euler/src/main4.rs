fn main() {
    let mut largest =0;
    for i in (1..1000).rev() {
        for j in (1..1000).rev() {
            if check_palindrome((i*j).to_string()) && i*j>largest{
                largest = i*j;
                println!("{}", i*j);
            }
        }
    }
}

fn check_palindrome (pospal: String) -> bool {
    pospal == pospal.chars().rev().collect::<String>()
}

