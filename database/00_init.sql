CREATE EXTENSION pg_trgm;

CREATE TABLE paper (
        id SERIAL UNIQUE,
        title TEXT NOT NULL,
        released DATE,
        dblp_url TEXT UNIQUE,
        title_vector TSVECTOR,
        CONSTRAINT "paper_id" PRIMARY KEY (id)
        );

CREATE TRIGGER paper_title_trigger
       BEFORE INSERT OR UPDATE ON paper
       FOR EACH ROW EXECUTE PROCEDURE tsvector_update_trigger(title_vector, 'pg_catalog.english', title);

CREATE INDEX paper_dblp_url ON paper(dblp_url);
CREATE INDEX paper_title ON paper(title);
CREATE INDEX paper_title_vector ON paper USING GIN (title_vector);
CREATE INDEX paper_title_trgm ON paper USING gist(title gist_trgm_ops);

CREATE TABLE author (
        id SERIAL UNIQUE,
        name TEXT NOT NULL,
        CONSTRAINT "author_id" PRIMARY KEY (id)
        );

CREATE INDEX author_name ON author(name);
CREATE INDEX author_name_trgm ON author USING gist(name gist_trgm_ops);


CREATE TABLE paper_author (
        id SERIAL UNIQUE,
        paper INTEGER NOT NULL,
        author INTEGER NOT NULL,
        UNIQUE (paper, author),
        CONSTRAINT "paper_author_id" PRIMARY KEY (id),
        CONSTRAINT "paper_author_paper" FOREIGN KEY (paper) REFERENCES paper (id) ON DELETE CASCADE,
        CONSTRAINT "paper_author_author" FOREIGN KEY (author) REFERENCES author (id) ON DELETE CASCADE
        );

CREATE TABLE paper_content (
        id SERIAL UNIQUE,
        paper INTEGER NOT NULL,
        source VARCHAR(255) NOT NULL,
        import_date TIMESTAMPTZ NOT NULL,
        full_body TEXT NOT NULL,
        CONSTRAINT "paper_content_id" PRIMARY KEY (id),
        CONSTRAINT "paper_content_paper" FOREIGN KEY (paper) REFERENCES paper (id) ON DELETE CASCADE
        );

CREATE TABLE paper_sentence (
        id SERIAL UNIQUE,
        paper_content INTEGER NOT NULL,
        sentence TEXT NOT NULL,
        CONSTRAINT "paper_sentence_id" PRIMARY KEY (id),
        CONSTRAINT "paper_sentence" FOREIGN KEY (paper_content) REFERENCES paper_content (id) ON DELETE CASCADE
        );

CREATE TABLE sentence_reference (
        id SERIAL UNIQUE,
        sentence INTEGER NOT NULL,
        paper INTEGER NOT NULL,
        CONSTRAINT "sentence_reference_id" PRIMARY KEY(id),
        CONSTRAINT "sentence_reference_sentence" FOREIGN KEY (sentence) REFERENCES paper_sentence (id) ON DELETE CASCADE,
        CONSTRAINT "sentence_reference_paper" FOREIGN KEY (paper) REFERENCES paper (id) ON DELETE CASCADE
        );
