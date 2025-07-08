import { defineConfig } from 'vite'
import RubyPlugin from 'vite-plugin-ruby'
import elmPlugin from 'vite-plugin-elm'

// if (process.env.RAILS_ENV !== 'test' && (process.env.NODE_ENV === 'production' || process.env.NODE_ENV === 'arthur')) {
//     elm = elmPlugin({
//         optimize: false, // no `--optimize` option when using elm-optimize-level-2
//         nodeElmCompilerOptions: {
//             pathToElm: 'node_modules/elm-optimize-level-2/bin/elm-optimize-level-2',
//             processOpts: "--optimize-speed"
//         }
//     })
// } else {
//     elm = elmPlugin({optimize: false})
// }

export default defineConfig({
  build: {
    assetsInlineLimit: 24576
  },
  plugins: [
    RubyPlugin(),
    elmPlugin()
  ],
})


//
// const config = {
//   build: {
//     assetsInlineLimit: 24576
//   },
//   plugins: [elmPlugin()]
// }
//
// export default config