import { defineConfig } from 'vite';
import RubyPlugin from 'vite-plugin-ruby';
import elmPlugin from 'vite-plugin-elm';

const isTest = process.env.RAILS_ENV === 'test'
const isProduction = process.env.NODE_ENV === 'production'
const isArthur = process.env.NODE_ENV === 'arthur'
// const isBuild = (process.env.RAILS_ENV !== 'test' && (process.env.NODE_ENV === 'production' || process.env.NODE_ENV === 'arthur'))
const isBuild= (isTest && !isProduction) || isProduction || isArthur

const elmOptions = isBuild ? {
        // optimize: false, // no `--optimize` option when using elm-optimize-level-2
        // nodeElmCompilerOptions: {
        //     pathToElm: 'node_modules/elm-optimize-level-2/bin/elm-optimize-level-2',
        //     processOpts: "--optimize-speed"
        // }
        optimize: true,
        nodeElmCompilerOptions: {
          verbose: true
        }
    } : {
      optimize: false,
      nodeElmCompilerOptions: {
        verbose: true
      }
    }

export default defineConfig({
  build: {
    assetsInlineLimit: 24576
  },
  plugins: [
    RubyPlugin(),
    // elmPlugin({
    //   nodeElmCompilerOptions: {
    //     verbose: true
    //   }
    // })
    // elmPlugin(elmOptions)
    elmPlugin()
  ],
})