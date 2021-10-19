.PHONY: frontend
frontend: frontend/node_modules
	rm -rf _build/prod/rel
	rm -rf priv/js
	cd frontend && npx shadow-cljs release app

.PHONY: release
release: frontend
	rebar3 release

.PHONY: image
image: frontend
	nix-build image.nix

.PHONY: frontend-dev
frontend-dev: frontend/node_modules
	cd frontend && npx shadow-cljs watch app

.PHONY: backend-dev
backend-dev:
	rebar3 shell

frontend/node_modules:
	cd frontend && npm install
