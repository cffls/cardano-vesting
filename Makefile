format: ## runs code style and formatter
	poetry run isort .
	poetry run black .

run-server:
	FEE_ADDRESS=addr_test1vrg8axv57lclxv7fwch7jyllz0xyjh32af3377grgegsn9gg67sky \
	MINT_UTXO_PATH=resource/preview/plutus_mint_script_utxo.pickle \
	VEST_UTXO_PATH=resource/preview/plutus_vest_script_utxo.pickle \
	FLASK_APP=cvest.server \
	FLASK_DEBUG=1 poetry run flask run

run-frontend:
	cd frontend && npm start