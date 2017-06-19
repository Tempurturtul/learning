const pgp = require('pg-promise')();
const config = require('../config');
const db = pgp(config.database);
const jwt = require('jsonwebtoken');

function getAllUsers(req, res, next) {
	db.any(`select * from usrs`)
		.then((data) => {
			res.json({
				status: 'success',
				data,
				message: 'Retrieved all users',
			});
		})
		.catch((err) => {
			return next(err);
		});
}

function authenticate(req, res, next) {
	const { name, password } = req.body;

	db.any(`select * from usrs where name = '${name}'`)
		.then((data) => {
			if (data.length == 0) {
				res.json({
					success: false,
					message: 'Authentication failed. User not found.',
				});
			}
			else {
				const user = data[0];

				// Check password.
				if (user.password !== password) {
					res.json({
						success: false,
						message: 'Authentication failed. Wrong password.',
					});
				}
				else {
					// Create a token.
					const token = jwt.sign(user, config.secret, {
						expiresIn: '24h', 	// Expires in 24 hours.
					});

					res.json({
						success: true,
						message: 'Token sent.',
						token,
					});
				}
			}
		})
		.catch((err) => {
			return next(err);
		});
}

module.exports = {
	getAllUsers,
	authenticate,
};
