# Build stage 0
#FROM erlang:alpine
#FROM erlang:21.0
FROM alpine:latest

# Install Rebar3
RUN mkdir -p /buildroot/rebar3/bin
COPY ./rebar3 /buildroot/rebar3/bin/rebar3
RUN chmod a+x /buildroot/rebar3/bin/rebar3

# Setup Environment
ENV PATH=/buildroot/rebar3/bin:$PATH

# Reset working directory
WORKDIR /buildroot

RUN apk add --no-cache git && \
    apk add --no-cache erlang erlang-crypto erlang-asn1 erlang-public-key \
                       erlang-ssl erlang-sasl erlang-dev

# Copy our Erlang test application
COPY imagemanager imagemanager

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
COPY --from=0 /buildroot/imagemanager/_build/prod/rel/imagemanager /imagemanager

RUN mkdir /uploads
ENV UPLOAD_PATH=/uploads
RUN mkdir /thumbnails
ENV THUMBNAIL_PATH=/thumbnails

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/imagemanager/bin/imagemanager", "foreground"]

