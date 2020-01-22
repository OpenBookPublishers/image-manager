#!/bin/bash

#APPNAME=$(basename $(pwd -P))
APPNAME=imagemanager
CONTAINER_BASE=$APPNAME
CONTAINER_BASE=$(basename $(pwd -P))
TAG=base-im
PGUSER=$USER

set -eu
cd $(dirname $0)

usage () {
    echo Usage: $0 command
    echo ''
    echo Read the source for the available commands
}

cleardb () { 
    docker exec -it ${APPNAME}_db_1 \
        psql -U $PGUSER starter -c 'delete from image;'
}

clearcache () {
    docker exec -it ${APPNAME}_web_1 \
        sh -c 'rm -f /uploads/* /thumbnails/*'
}

if [ $# != 1 ]; then
    usage >&2
    exit 1
fi

case "$1" in
    clear-db)
        cleardb
        ;;
    clear-cache)
        clearcache
        ;;
    clear-all)
        cleardb
        clearcache
        ;;
    rebuild-client)
        (cd app/${APPNAME}/client && npm run build)
        ;;
    install-client)
        (cd app/${APPNAME}/client && ./postbuild.sh)
        ;;
    install-client-deps)
        (cd app/${APPNAME}/client && npm install)
        ;;
    rebuild-server)
        docker build app -t $TAG
        ;;
    rebuild-release)
        (cd app/${APPNAME}/ && rebar3 release)
        ;;
    start-server)
        docker-compose up
        ;;
    restart-server)
        docker-compose down
        docker-compose up
        ;;
    stop-server)
        docker-compose down
        ;;
    erlang-console)
        docker exec -it ${CONTAINER_BASE}_web_1 /${APPNAME}/bin/${APPNAME} remote_console
        ;;
    shell)
        docker exec -it ${CONTAINER_BASE}_web_1 sh
        ;;
    psql)
        docker exec -it ${CONTAINER_BASE}_db_1 psql -U $USER starter
        ;;
    refresh-schema)
        docker-compose down || true
        docker volume rm ${CONTAINER_BASE}_db
        docker-compose up
        ;;
    *)
        echo No commands currently implemented
        ;;
esac

exit

## NOTES

# this ${APPNAME} needs to match the name repeatedly used in the Dockerfile
rebar3 new release ${APPNAME}

# postgres connection environment details need to plumbed deliberately
# e.g., using db.env
