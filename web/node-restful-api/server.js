const express = require('express');
const app = express();
const bodyParser = require('body-parser');
const port = process.env.PORT || 3000;
const apiRouter = require('./routes/api');

// Use body-parser to parse forms and json in requests.
app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());

app.use('/api', apiRouter);

// Error handling.
app.use((err, req, res, next) => {
	res.status(err.status || 500)
		.json({
			status: 'error',
			message: err.message,
		});
});

// Start the server.
app.listen(port, () => {
	console.log(`Listening on port ${port}.`);
});
