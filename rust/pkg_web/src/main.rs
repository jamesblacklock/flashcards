use std::io::{Read, Write};

fn main() {
	let mut args = std::env::args().skip(1);
	let in_file = args.next().unwrap();
	let out_file = args.next().unwrap();
	let mut wasm = Vec::new();
	std::fs::File::open(in_file).unwrap().read_to_end(&mut wasm).unwrap();
	let encoded = base64::encode(&wasm);
	let mut out_file = std::fs::File::create(out_file).unwrap();
	writeln!(out_file,
"const wasmEncoded = \"data:application/wasm;base64,{encoded}\";
export const loadRuntime = (imports: any) => WebAssembly.instantiateStreaming(fetch(wasmEncoded), imports);").unwrap();
}
