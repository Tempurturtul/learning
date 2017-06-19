const pgp = require('pg-promise')();
const connectionConfig = {
	host: 'localhost',
	port: 5432,
	database: 'puppies',
	user: 'postgres',
	password: 'foobar',
};
const db = pgp(connectionConfig);

function getAllPuppies(req, res, next) {
	// `any` is a Query Result Mask that means any number of result rows
	// are expected. Others are `one`, `many` and `none`.
	db.any('select * from pups')
		.then((data) => {
			res.json({
				status: 'success',
				data,
				message: 'Retrieved all puppies',
			});
		})
		.catch((err) => {
			return next(err);
		});
}

function getPuppy(req, res, next) {
	const pupID = parseInt(req.params.id);
	db.one(`select * from pups where id = ${pupID}`)
		.then((data) => {
			res.json({
				status: 'success',
				data,
				message: 'Retrieved one puppy',
			});
		})
		.catch((err) => {
			return next(err);
		});
}

function createPuppy(req, res, next) {
	const { name, breed, age, sex } = req.body;

	db.none(`insert into pups(name, breed, age, sex)
		values ('${name}', '${breed}', ${age}, '${sex}')`)
		.then(() => {
			res.json({
				status: 'success',
				message: 'Inserted one puppy',
			});
		})
		.catch((err) => {
			return next(err);
		});
}

function updatePuppy(req, res, next) {
	const id = req.params.id;
	const { name, breed, age, sex } = req.body;

	db.none(`update pups set name='${name}', breed='${breed}', age=${age},
		sex='${sex}' where id=${id}`)
		.then(() => {
			res.json({
				status: 'success',
				message: 'Updated puppy',
			});
		})
		.catch((err) => {
				return next(err);
		});
}

function removePuppy(req, res, next) {
	const id = req.params.id;

	// result returns the number of records affected by the query.
	db.result(`delete from pups where id=${id}`)
		.then((result) => {
			res.json({
				status: 'success',
				message: `Removed ${result.rowCount} puppies`,
			});
		})
		.catch((err) => {
			return next(err);
		});
}

module.exports = {
	getAllPuppies,
	getPuppy,
	createPuppy,
	updatePuppy,
	removePuppy,
};
