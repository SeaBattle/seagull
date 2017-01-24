PROJECT = seaconfig

DEPS = jsone
dep_jsone = git https://github.com/sile/jsone.git v0.3.3

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git 0.8.4

include erlang.mk
