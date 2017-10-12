CREATE TYPE Duration AS ENUM ('SMALL', 'MEDIUM', 'LONG');
CREATE TYPE Genre AS ENUM ('FICTION', 'NON-FICTION');

CREATE TABLE tags(
  id         SERIAL PRIMARY KEY,
  name       VARCHAR(50)              NOT NULL,
  genre      Genre                    NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);


CREATE TABLE authors(
  id         SERIAL PRIMARY KEY,
  name       VARCHAR(100),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);


CREATE TABLE stories(
  id         SERIAL PRIMARY KEY,
  title      VARCHAR(50)                   NOT NULL,
  duration   Duration                      NOT NULL,
  author     INT  REFERENCES authors (id),
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

