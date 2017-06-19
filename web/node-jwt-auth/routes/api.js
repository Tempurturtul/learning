const router = require('express').Router();
const db = require('../db');
const config = require('../config');
const jwt = require('jsonwebtoken');

// Unprotected routes.
router.post('/authenticate', db.authenticate);

// Route middleware to verify tokens.
router.use((req, res, next) => {
	// Check for a token.
	const token = req.body.token || req.query.token || req.headers['x-access-token'];

	if (token) {
		// Decode the token.
		jwt.verify(token, config.secret, (err, decoded) => {
			if (err) {
				res.json({
					sucess: false,
					message: 'Failed to authenticate token.',
				});
			}
			else {
				req.decoded = decoded;
				next();
			}
		});
	}
	else {
		// No token. (403 Forbidden)
		res.status(403).json({
			success: false,
			message: 'No token provided.',
		});
	}
});

// Protected routes.
router.get('/users', db.getAllUsers);

module.exports = router;
