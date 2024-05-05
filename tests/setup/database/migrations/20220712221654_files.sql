CREATE TABLE IF NOT EXISTS users
(
    id       BIGSERIAL PRIMARY KEY,
    flubber  INTEGER NOT NULL,
    username TEXT NOT NULL
);

INSERT INTO users (flubber, username) VALUES (1, 'johndoe');