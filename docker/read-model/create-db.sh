#!/bin/bash
set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
    CREATE TABLE offsets (
        latest bigint PRIMARY KEY
    );

    INSERT INTO offsets (latest) VALUES (-1);

    CREATE TABLE game_state (
        value text PRIMARY KEY
    );

    INSERT INTO game_state (value) VALUES
        ('in_progress'),
        ('draw'),
        ('yellow_won'),
        ('red_won');

    CREATE TABLE color (
        value text PRIMARY KEY
    );

    INSERT INTO color (value) VALUES
        ('red'),
        ('yellow');    

    CREATE TABLE player (
        value text PRIMARY KEY
    );    

    INSERT INTO player (value) VALUES
        ('red'),
        ('yellow'),
        ('none');

    CREATE TABLE games_internal(
        id uuid NOT NULL PRIMARY KEY,
        serial_id serial NOT NULL,
        game_state text NOT NULL REFERENCES game_state(value),
        moves integer[] NOT NULL,
        player_red uuid NOT NULL,
        player_yellow uuid NOT NULL
    );

    CREATE TABLE games(
        id uuid NOT NULL PRIMARY KEY,
        serial_id serial NOT NULL,
        game_state text NOT NULL REFERENCES game_state(value),
        moves integer[] NOT NULL,
        player text NOT NULL REFERENCES player(value)
    );    

    CREATE FUNCTION game(
        my_id uuid,
        game_id uuid)
    RETURNS SETOF games AS
    '   SELECT id,
               serial_id,
               game_state,
               moves,
               CASE WHEN my_id = player_red THEN ''red''
                    WHEN my_id = player_yellow THEN ''yellow''
                    ELSE ''none''
               END
        FROM games_internal
        WHERE id = game_id
    ' LANGUAGE sql STABLE;    

    CREATE FUNCTION live_games(my_id uuid)
    RETURNS SETOF games AS
    '   SELECT id, serial_id, game_state, moves, ''none''
        FROM games_internal
        WHERE player_red <> my_id AND player_yellow <> my_id
    ' LANGUAGE sql STABLE;    

    CREATE FUNCTION my_games(my_id uuid)
    RETURNS SETOF games AS
    '   SELECT id,
               serial_id,
               game_state,
               moves,
               CASE WHEN my_id = player_red THEN ''red''
                    WHEN my_id = player_yellow THEN ''yellow''
                    ELSE ''none''
               END
        FROM games_internal
        WHERE player_red = my_id OR player_yellow = my_id
    ' LANGUAGE sql STABLE;     

    CREATE TABLE challenges_internal(
        id uuid NOT NULL PRIMARY KEY,
        serial_id serial NOT NULL,
        client_id uuid NOT NULL,
        color text NOT NULL REFERENCES color(value)
    );

    CREATE TABLE challenges(
        id uuid NOT NULL PRIMARY KEY,
        serial_id serial NOT NULL,
        color text NOT NULL REFERENCES color(value)
    );

    CREATE FUNCTION villain_challenges(my_id uuid)
    RETURNS SETOF challenges AS
    '   SELECT id, serial_id, color
        FROM challenges_internal
        WHERE client_id <> my_id
    ' LANGUAGE sql STABLE;    

    CREATE FUNCTION my_challenges(my_id uuid)
    RETURNS SETOF challenges AS
    '   SELECT id, serial_id, color
        FROM challenges_internal
        WHERE client_id = my_id
    ' LANGUAGE sql STABLE;
EOSQL
