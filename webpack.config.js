const webpack = require('webpack');
const path = require('path');
const plugins = [];
// const UglifyJSPlugin = require('uglifyjs-webpack-plugin');

module.exports = {
  entry: './src/Main.purs',
  target: 'web',
  output: {
    path: path.resolve(__dirname, 'dist'),
    pathinfo: true,
    filename: 'main.js',
    // sourceMapFilename: '../dist/m3u-app.js.map',
    library: "App",
    libraryTarget: "var"
  },
  module : {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [
                'bower_components/purescript-*/src/**/*.purs',
                'src/**/*.purs'
              ],
              bundle: false,
              psc: 'psa',
              watch: true,
              pscIde: false
            }
          }
        ]
      }
    ]
  },
  resolve: {
    modules: [ 'node_modules', 'bower_components' ],
    extensions: [ '.purs', '.js']
  },
  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    })
  ].concat(plugins),

  // resolve: {
  //   // Pull in vue
  //   alias: {
  //     vue: './vue.min.js',
  //     jsmediatags : './jsmediatags.js',
  //     ps : './ps.js',
  //   }
  // },
  devServer: {
    contentBase: path.join(__dirname, "dist"),
    port: 9000,
  },
  devtool : "#source-map"
};
