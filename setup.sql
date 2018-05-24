CREATE TYPE Genre AS ENUM ('FICTION', 'NON-FICTION');

CREATE TABLE tags(
  id         SERIAL PRIMARY KEY,
  name       VARCHAR(50)              NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);


CREATE TABLE authors(
  id         SERIAL PRIMARY KEY,
  name       VARCHAR(100),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE users(
  id          SERIAL PRIMARY KEY,
  username    VARCHAR(100),
  displayname VARCHAR(100),
  password    bytea,
  author      INT  REFERENCES authors (id),
  salt        bytea,
  created_at  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE sessions(
  id         SERIAL PRIMARY KEY,
  session_id VARCHAR(256),
  user_      INT REFERENCES users (id),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE stories(
  id         SERIAL PRIMARY KEY,
  title      VARCHAR(50)                   NOT NULL,
  times_read INT                           NOT NULL DEFAULT 0,
  stars      INT,
  genre      Genre                         NOT NULL,
  story      TEXT,
  created_at TIMESTAMP WITH TIME ZONE      NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE      NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE story_tags(
  id       SERIAL PRIMARY KEY,
  story_id INT REFERENCES stories (id),
  tag_id   INT REFERENCES tags (id)
);

CREATE TABLE story_authors(
  id        SERIAL PRIMARY KEY,
  story_id  INT REFERENCES stories (id),
  author_id INT REFERENCES authors (id)
);
