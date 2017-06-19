# Node.js RESTful API Example

Using Node.js, Express, PostgreSQL, and pg-promise.

## API Endpoints

| URL			| HTTP Verb	| Action		|
|-----------------------|---------------|-----------------------|
| /api/puppies		| GET		| Return ALL puppies	|
| /api/puppies/:id	| GET		| Return a puppy	|
| /api/puppies		| POST		| Add a puppy		|
| /api/puppies/:id	| PUT		| Update a puppy	|
| /api/puppies/:id	| DELETE	| Delete a puppy	|

## Notes

To create a fresh database for testing:

- `sudo -u postgres psql -f db/puppies.sql`

## Example Source

http://mherman.org/blog/2016/03/13/designing-a-restful-api-with-node-and-postgres/

and

https://scotch.io/tutorials/build-a-restful-api-using-node-and-express-4
