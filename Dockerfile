FROM alpine:3.6 as alpine
RUN apk add -U --no-cache ca-certificates

FROM fpco/haskell-scratch:integer-gmp
COPY --from=alpine /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/