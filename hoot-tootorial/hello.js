async function load() {
    const [message] = await Scheme.load_main("hello.wasm", {});
    console.log(message);
}

window.addEventListener("load", load);
