(ql:quickload :postmodern)

(add-package-local-nickname :pg :postmodern)

(pg:connect-toplevel "omaha_store_development" "liodev" "123" "localhost")

(pg:query "select * from binaries limit 1")

(pg:disconnect-toplevel)
