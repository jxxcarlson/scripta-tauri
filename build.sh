elm make src/Main.elm --output=out/main.js --optimize
cp public/index.html out/
cp public/katex-custom-element.js out/
cp public/tauri.js out/
cp public/katex.min.js out/
cp public/katex.min.css out/
cp -r public/fonts out/
