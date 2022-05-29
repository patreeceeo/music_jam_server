const esbuild = require('esbuild')
const chokidar = require("chokidar");
const { execSync } = require("child_process");

// Decide which mode to proceed with
let mode = 'build'
process.argv.slice(2).forEach((arg) => {
  if (arg === '--watch') {
    mode = 'watch'
  } else if (arg === '--deploy') {
    mode = 'deploy'
  }
})

// Define esbuild options + extras for watch and deploy
let opts = {
  entryPoints: ['js/app.js'],
  bundle: true,
  logLevel: 'info',
  target: 'es2017',
  outdir: '../priv/static/assets'
}
if (mode === 'watch') {
  opts = {
    watch: true,
    sourcemap: 'inline',
    ...opts
  }
}
if (mode === 'deploy') {
  opts = {
    minify: true,
    ...opts
  }
}

// Start esbuild with previously defined options
// Stop the watcher when STDIN gets closed (no zombies please!)
esbuild.build(opts).then((result) => {
  if (mode === 'watch') {
    process.stdin.pipe(process.stdout)
    process.stdin.on('end', () => { result.stop() })
  }
}).catch((error) => {
  process.exit(1)
})



// Exit the process when standard input closes due to:
//   https://hexdocs.pm/elixir/1.10.2/Port.html#module-zombie-operating-system-processes
//
process.stdin.on("end", function() {
    console.log("standard input end");
    process.exit();
});

process.stdin.resume();

// Set up chokidar to watch all elm files and rebuild the elm app ignoring process errors
chokidar.watch("elm/**/*.elm", { ignored: "node_modules" }).on("all", (event, path) => {
    console.log(event, path);
    try {
        execSync("./node_modules/.bin/elm make elm/Main.elm --output=../priv/static/assets/Elm.Main.js");
    } catch (error) {}
});
