# cardano-vesting


## Steps to run full stack

### Prepare keys

```shell
poetry run python scripts/deploy_contract.py
```

### Start backend server

```shell
BLOCKFROST_PREVIEW=<preview_project_id> BLOCKFROST_PREPROD=<preprod_project_id> make run-server
```

### Start frontend server

```shell
make run-frontend
```
