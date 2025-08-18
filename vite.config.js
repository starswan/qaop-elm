import { defineConfig } from 'vite';
import RubyPlugin from 'vite-plugin-ruby';
import elmPlugin from 'vite-plugin-elm';

// const isBuild = (process.env.RAILS_ENV !== 'test' && (process.env.NODE_ENV === 'production' || process.env.NODE_ENV === 'arthur'))
const isBuild= (process.env.NODE_ENV === 'production' || process.env.NODE_ENV === 'arthur')

const elmOptions = isBuild ? {
        // optimize: false, // no `--optimize` option when using elm-optimize-level-2
        // nodeElmCompilerOptions: {
        //     pathToElm: 'node_modules/elm-optimize-level-2/bin/elm-optimize-level-2',
        //     processOpts: "--optimize-speed"
        // }
        optimize: true,
        nodeElmCompilerOptions: {
        }
    } : {optimize: false}

export default defineConfig({
  build: {
    assetsInlineLimit: 24576
  },
  plugins: [
    RubyPlugin(),
    // elmPlugin({
    //   optimize: false,
    //   nodeElmCompilerOptions: {
    //     verbose: true
    //   }
    // })
    elmPlugin(elmOptions)
  ],
})