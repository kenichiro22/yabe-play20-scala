# --- !Ups
CREATE TABLE Tag (
    id bigint(20) NOT NULL AUTO_INCREMENT,
    name VARCHAR(255) NOT NULL,
    PRIMARY KEY (id)
);
# --- !Downs
DROP TABLE Tag;
