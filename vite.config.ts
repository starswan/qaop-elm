import { defineConfig } from 'vite'
import RubyPlugin from 'vite-plugin-ruby'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  build: {
    assetsInlineLimit: 24576
  },
  plugins: [
    RubyPlugin(),
      elmPlugin({
          optimize: false, // no `--optimize` option when using elm-optimize-level-2
          nodeElmCompilerOptions: {
              pathToElm: process.env.NODE_ENV === 'production' ? 'node_modules/elm-optimize-level-2/bin/elm-optimize-level-2' : undefined
          }
      })
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