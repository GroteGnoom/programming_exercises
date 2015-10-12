use std::io;
use std::process;

fn main() {
	let mut string_stack: Vec<String> = vec![];
	let mut float_stack: Vec<f64> = vec![];
	let mut int_stack: Vec<i64> = vec![];
	loop {
		let mut input = String::new();
		
		println!("String stack: {:?}", string_stack);
		println!("Float stack: {:?}", float_stack);
		println!("Int stack: {:?}", int_stack);
	
		println!("input:");

	

		io::stdin().read_line(&mut input)
			.ok()
			.expect("Failed to read line!");

		let input = input.trim();

		if input == "$" {
			let execute = string_stack.pop().unwrap();
			execute_string (execute, &mut string_stack, &mut float_stack, &mut int_stack);	
		} else {
			string_stack.push(input.to_string());
		}

	}
}

fn execute_string (execute: String, string_stack: &mut Vec<String>, float_stack: &mut Vec<f64>, int_stack: &mut Vec<i64>) { 
	if execute == "q" {
		process::exit(1);
	} else if execute == "f" {
		float_stack.push(string_stack.pop().unwrap().parse::<f64>().unwrap());
	} else if execute == "f+" {
		let a = float_stack.pop().unwrap();
		let b = float_stack.pop().unwrap();
		float_stack.push(a+b);
	} else if execute == "i" {
		int_stack.push(string_stack.pop().unwrap().parse::<i64>().unwrap());
	} else if execute == "i+" {
		let a = int_stack.pop().unwrap();
		let b = int_stack.pop().unwrap();
		int_stack.push(a+b);
	} else if execute == "ic" {
		let times = int_stack.pop().unwrap();
		let to_copy = int_stack.pop().unwrap();
		for _i in 0..times {
			int_stack.push(to_copy);
		}
	} else if execute == "for" {
		let from = int_stack.pop().unwrap();
		let to = int_stack.pop().unwrap();
		let execute_for = string_stack.pop().unwrap();
		for i in from..(to+1) {
			println!("in for-loop, i is: {}",i);
			execute_string (execute_for.clone(), string_stack, float_stack, int_stack);	
		}
	} else {
		 println!("not executable");
	} 
}
