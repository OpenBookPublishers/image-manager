FROM node AS npm

WORKDIR /build

COPY client/package-lock.json ./
COPY client/package.json ./
RUN npm install --no-cache
COPY client/src ./src
COPY client/public ./public
RUN npm run build

# Build stage 0
FROM alpine:latest AS erlangbuild

# Install Rebar3
WORKDIR /buildroot/rebar3/bin
RUN wget -O rebar3 https://s3.amazonaws.com/rebar3/rebar3
RUN chmod a+x rebar3

# Setup Environment
ENV PATH=/buildroot/rebar3/bin:$PATH

# Reset working directory
WORKDIR /buildroot

RUN apk add --no-cache git && \
    apk add --no-cache erlang erlang-crypto erlang-asn1 erlang-public-key \
                       erlang-ssl erlang-sasl erlang-dev

# Copy our Erlang test application
COPY imagemanager imagemanager

COPY --from=npm /build/build imagemanager/apps/imagemanager/priv/public

# And build the release
WORKDIR imagemanager
RUN rebar3 as prod release


# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl \
                       ncurses-libs \
                       imagemagick

# Install the released application
COPY --from=erlangbuild /buildroot/imagemanager/_build/prod/rel/imagemanager /imagemanager

RUN mkdir /uploads
ENV UPLOAD_PATH=/uploads
RUN mkdir /thumbnails
ENV THUMBNAIL_PATH=/thumbnails

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/imagemanager/bin/imagemanager", "foreground"]

