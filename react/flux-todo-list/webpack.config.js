module.exports = {
  context: __dirname + '/app',
  entry: './js/app.js',
  output: {
    path: __dirname + '/dist/js',
    publicPath: '/js/',
    filename: 'bundle.js'
  },
  devServer: {
    inline: true,
    port: 3333,
    contentBase: './app'
  },
  module: {
    loaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: 'babel-loader',
        query: {
          presets: ['react']
        }
      }
    ]
  }
}
