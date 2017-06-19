const router = require('express').Router();
const db = require('../db');

// Routes.
router.get('/puppies', db.getAllPuppies);
router.get('/puppies/:id', db.getPuppy);
router.post('/puppies', db.createPuppy);
router.put('/puppies/:id', db.updatePuppy);
router.delete('/puppies/:id', db.removePuppy);

module.exports = router;
