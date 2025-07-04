cd jazz
cargo build --lib --target=wasm32-unknown-unknown
cd ../pkg_web
mkdir -p ../dist
cargo run -- ../../target/wasm32-unknown-unknown/debug/jazz.wasm ../../examples/flashcards/jazz_wasm.ts
