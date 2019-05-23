var path = require('path');

module.exports = {
    entry: './src/static/index.js',
    output: {
        path: __dirname + "./dist",
        filename: 'bundle.js'
    },
    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use : {
                    loader:  'elm-webpack-loader',
                    options: { verbose: true }
                } 
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: "file-loader"
            },
            {
                test: /\.scss$/,
                exclude: [/elm-stuff/, /node_modules/],
                loaders: ["style-loader", "css-loader?url=false", "sass-loader"]
            },
            {
                test: /\.css$/,
                exclude: [/elm-stuff/, /node_modules/],
                loaders: ["style-loader", "css-loader?url=false"]
            }
        ]
    },
    resolve: {
        extensions: ['*', '.js', '.elm']
    },
    devServer: {
        inline: true,
        contentBase: path.join(__dirname, './src/static'),
        compress: true,
        port: 3000
    }
}
