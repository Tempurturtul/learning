DROP DATABASE IF EXISTS users;
CREATE DATABASE users;

\c users;

CREATE TABLE usrs (
	name VARCHAR PRIMARY KEY,
	password VARCHAR,
	admin BOOLEAN
);

INSERT INTO usrs (name, password, admin)
	VALUES ('Tempurturtul', 'foobar', TRUE);
