elm make src/Main.elm --output=out/main1.js --optimize
npx uglifyjs out/main1.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' --output out/main2.js
npx uglifyjs out/main2.js --mangle --output out/main.js
rm out/main1.js
rm out/main2.js
cp public/index.html out/
cp public/katex-custom-element.js out/
cp public/tauri.js out/
cp public/katex.min.js out/
cp public/katex.min.css out/
cp -r public/fonts out/
cp public/codemirror-element.js out/
