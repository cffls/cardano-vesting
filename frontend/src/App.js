import React from 'react';
import cbor from "cbor";
import { Buffer } from 'buffer';
import * as CardanoWasm from "@emurgo/cardano-serialization-lib-asmjs";


class PendingRecipientList extends React.Component {
  render () {
    var items = this.props.items.map((item, index) => {
      return (
        <RecipientListItem key={index} item={item} index={index} removeItem={this.props.removeItem}/>
      );
    });

    return (
      <table className="table table-hover">
        <thead>
          <tr>
            <th style={{width: "5%"}} scope="col">#</th>
            <th style={{width: "40%"}} scope="col">Recipient</th>
            <th style={{width: "15%"}} scope="col">Amount</th>
            <th style={{width: "25%"}} scope="col">Vest date (UTC)</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
        {items}
        <RecipientForm items={this.props.items} addItem={this.props.addItem} />
        </tbody>
      </table>
    );
  }
}

class VestList extends React.Component {
  render () {
    var items = this.props.items.map((item, index) => {
      return (
        <VestListItem key={index} item={item} index={index}/>
      );
    });

    return (
      <table className="table table-bordered table-hover">
        <thead>
          <tr>
            <th style={{width: "5%"}} scope="col">#</th>
            <th style={{width: "10%"}} scope="col">Amount</th>
            <th style={{width: "20%"}} scope="col">Vest date (UTC)</th>
            <th style={{width: "30%"}} scope="col">Sender</th>
            <th style={{width: "12%"}} scope="col">Cancellable</th>
          </tr>
        </thead>
        <tbody>
        {items}
        </tbody>
      </table>
    );
  }
}

class VestListItem extends React.Component {

  render () {
    return(
      <tr>
        <th scope="row">{this.props.index+1}</th>
        <td >{parseFloat(this.props.item.amount)/1000000} ₳</td>
        <td >{this.props.item.deadline}</td>
        <td >{this.props.item.granter}</td>
        <td >{this.props.item.cancellable? "Yes":"No"}</td>
      </tr>
    );
  }
}

class GrantList extends React.Component {
  render () {
    var items = this.props.items.map((item, index) => {
      return (
        <GrantListItem key={index} item={item} index={index}/>
      );
    });

    return (
      <table className="table table-bordered table-hover">
        <thead>
          <tr>
            <th style={{width: "5%"}} scope="col">#</th>
            <th style={{width: "10%"}} scope="col">Amount</th>
            <th style={{width: "20%"}} scope="col">Vest date (UTC)</th>
            <th style={{width: "30%"}} scope="col">Recipient</th>
            <th style={{width: "12%"}} scope="col">Cancellable</th>
          </tr>
        </thead>
        <tbody>
        {items}
        </tbody>
      </table>
    );
  }
}

class GrantListItem extends React.Component {

  render () {
    return(
      <tr>
        <th scope="row">{this.props.index+1}</th>
        <td >{parseFloat(this.props.item.amount)/1000000} ₳</td>
        <td >{this.props.item.deadline}</td>
        <td >{(this.props.item.beneficiary.length) > 0? this.props.item.beneficiary: this.props.item.beneficiary_script}</td>
        <td >{this.props.item.cancellable? "Yes":"No"}</td>
      </tr>
    );
  }
}

class RecipientListItem extends React.Component {
  constructor(props) {
    super(props);
    this.onClickClose = this.onClickClose.bind(this);
  }

  onClickClose() {
    let index = parseInt(this.props.index);
    this.props.removeItem(index);
  }

  render () {
    return(
      <tr>
        <th scope="row">{this.props.index+1}</th>
        <td>{this.props.item.value.addressValue}</td>
        <td>{this.props.item.value.amountValue} ₳</td>
        <td>{new Date(this.props.item.value.deadlineValue).toISOString().split(":").slice(0, 2).join(":")}</td>
        <td >
          <button className="btn btn-sm btn-danger" type="button" onClick={this.onClickClose}>&times;</button>
        </td>
      </tr>
    );
  }
}

class RecipientForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      address: React.createRef(),
      amount: React.createRef(),
      deadline: React.createRef(),
      curTime: new Date().toISOString().split(":").slice(0, 2).join(":"),
      form: React.createRef()
    }
  }

  async onSubmit(event) {
    event.preventDefault();
    let addressValue = this.state.address.current.value;

    try {
      await CardanoWasm.Address.from_bech32(addressValue);
    } catch(err) {
      alert("Invalid Cardano address: " + addressValue);
      return;
    }

    let amountValue = this.state.amount.current.value;

    if (!/^[0-9]+(.[0-9]{1,6})?$/.test(amountValue)) {
      alert("Invalid ADA amount: " + amountValue);
      return;
    }

    if (parseFloat(amountValue) < 1.5) {
      alert("Minimum ADA amount is 1.5");
      return;
    }

    let deadlineValue = Date.parse(this.state.deadline.current.value);

    if (deadlineValue < Date.now()) {
      alert("Vest date must be in the future");
      return;
    }

    if(addressValue) {
      this.props.addItem({addressValue, amountValue, deadlineValue});
      this.state.address.current.value = "";
      this.state.amount.current.value = "";
      this.state.deadline.current.value = "";
      this.state.form.current.reset();
    }
  }

  render () {
    return (
      <tr>
        <th scope="row">{this.props.items.length+1}</th>
        <td>
          <input type="text" className="form-control" ref={this.state.address} placeholder="Cardano address"/>
        </td>
        <td>
          <input type="text" className="form-control" ref={this.state.amount} placeholder="Amount"/>
        </td>
        <td>
          <input type="datetime-local" className="form-control" min={this.state.curTime} ref={this.state.deadline}/>
        </td>
        <td>
          <form ref={this.state.form} onSubmit={(event) => this.onSubmit(event)} className="form-inline">
            <button type="submit" className="btn btn-light">Add</button>
          </form>
        </td>
      </tr>
    );
  }
}

class App extends React.Component {
  constructor (props) {
    super(props);
    this.state = {
      pendingRecipients: [],
      vests: [],
      grants: [],
      connected: false,
      selectedTab: "create",
      balance: 0,
      wallet: null,
      walletName: "",
      icon: null,
      pendingTx: false,
    };

    this.addItem = this.addItem.bind(this);
    this.removeItem = this.removeItem.bind(this);
    this.submitGrantRequest = this.submitGrantRequest.bind(this);
    this.prepare_sender = this.prepare_sender.bind(this);
    this.connectWallet = this.connectWallet.bind(this);
    this.signTx = this.signTx.bind(this);
    this.sendTxAndWitnessBack = this.sendTxAndWitnessBack.bind(this);
  }

  async componentDidMount() {
    let wallets = ["nami", "eternl"];

    for (let i = 0; i < wallets.length; i++) {
      let wallet = wallets[i];
      let enabled = await window.cardano[wallet].isEnabled();
      if (enabled) {
        let api = await window.cardano[wallet].enable();
        this.setState({wallet: api, walletName: wallet, connected: true}, () => {
          this.updateWalletInfo();
          this.updateVestList();
          this.updateGrantList();
        });

        break;
      }
    }
  }

  async updateWalletInfo() {
    this.setState({icon: window.cardano[this.state.walletName].icon});
    this.state.wallet.getBalance().then((balance) => {
      this.setState({balance: cbor.decodeAllSync(balance) / 1000000});
    });
  }

  selectTab(tabName) {
    this.setState({selectedTab: tabName})
  }

  addItem(recipientItem) {
    this.setState(prevState => ({
      pendingRecipients: [...prevState.pendingRecipients, {
        index: this.state.pendingRecipients.length+1,
        value: recipientItem,
      }]
    }))
  }

  removeItem (itemIndex) {
    var items = [...this.state.pendingRecipients];
    items.splice(itemIndex, 1);
    this.setState({pendingRecipients: items});
  }

  async updateVestList() {
    let usedAddresses = await this.state.wallet.getUsedAddresses();
    let unusedAddresses = await this.state.wallet.getUnusedAddresses();
    let response = await fetch("http://127.0.0.1:5000/get_vests?"+ new URLSearchParams({
        address: usedAddresses+unusedAddresses,
    }), {
      method: "GET",
    });

    let r = await response.json();

    if (r.results.length > 0) {
      this.setState({vests: r.results});
    }
  }

  async updateGrantList() {
    let usedAddresses = await this.state.wallet.getUsedAddresses();
    let response = await fetch("http://127.0.0.1:5000/get_grants?"+ new URLSearchParams({
        address: usedAddresses,
    }), {
      method: "GET",
    });

    let r = await response.json();

    if (r.results.length > 0) {
      this.setState({grants: r.results});
    }
  }

  async submitGrantRequest(senders, change_address) {
    try {
      let response = await fetch('http://127.0.0.1:5000/create_grants', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(
          {
            'senders': senders,
            'change_address': change_address,
            'grants': this.state.pendingRecipients.map((item, index) => {
              return {
                'address': item.value.addressValue,
                'amount': item.value.amountValue,
                'deadline': item.value.deadlineValue
              };
            })
          }
        )
      })
      await this.signTx(await response.json());
      this.state.pendingRecipients = [];
    } catch(error) {
      console.log(error);
    }
  }

  async signTx(tx) {
    let witness = await this.state.wallet.signTx(tx['tx']);
    await this.sendTxAndWitnessBack(tx['tx'], witness);
  }

  async sendTxAndWitnessBack(tx, witness) {
    fetch('http://127.0.0.1:5000/submit_tx', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(
        {
          'tx': tx,
          'witness': witness
        }
      )
    })
    .then(response => response.json())
    .then(data => {
        alert("Transaction: " + data["tx_id"] + " submitted!");
    })
  }

  async prepare_sender() {
    this.setState({pendingTx: true});
    let usedAddresses = await this.state.wallet.getUsedAddresses();
    let changeAddress = await this.state.wallet.getChangeAddress();
    await this.submitGrantRequest(usedAddresses, changeAddress);
    this.setState({pendingTx: false});
  }

  connectWallet(wallet) {
    if (!this.state.connected) {
      window.cardano[wallet].enable().then(
        (api) => {
          this.setState(
            {wallet: api, walletName: wallet, connected: true}, () => {
            this.updateWalletInfo();
            this.updateVestList();
            this.updateGrantList();
          });
        }
      ).catch((err) => {
        console.log("User rejected wallet connection", err);
      });
    }
  }
  render() {
    return (
      <div>
        <div>
          <nav className="navbar navbar-expand-lg navbar-light bg-light">
            <div className="container-fluid">
              <h3 className="navbar-brand ms-3">Cardano Vesting</h3>
              <div>
                {this.state.connected ? (
                  <div className="container">
                    <img style={{"width": "25px", "float": "left", "marginRight": "10px"}}
                               src={this.state.icon} alt=""/>
                    <div style={{"float": "left"}}>{this.state.balance} ₳</div>
                  </div>
                ) : (
                  <div className="dropdown me-3">
                    <button className="btn btn-light dropdown-toggle"
                            type="button"
                            data-bs-toggle="dropdown"
                            aria-expanded="false">
                      Connect Wallet
                    </button>
                    <ul className="dropdown-menu" >
                      <li className="dropdown-item">
                        <div className="wallet-icon" onClick={() => this.connectWallet("nami")}>
                          <img style={{"width": "25px", "float": "left", "marginRight": "10px"}}
                               src="nami.svg" alt=""/>
                          <div>Nami</div>
                        </div>
                      </li>
                      <li className="dropdown-item">
                        <div className="wallet-icon" onClick={() => this.connectWallet("eternl")}>
                          <img style={{"width": "25px", "float": "left", "marginRight": "10px"}}
                               src="eternl.svg" alt=""/>
                          <div>Eternl</div>
                        </div>
                      </li>
                    </ul>
                  </div>
                )}
              </div>
            </div>
          </nav>
        </div>

        <div className="container">
          <div className="main card">
            <div className="card-body">
              <div className="card-title">
                <ul className="nav nav-tabs">
                  <li className="nav-item">
                    <div className={"nav-link " + ((this.state.selectedTab === "create") ? "active": "")}
                      onClick={() => this.selectTab("create")}>Create</div>
                  </li>
                  <li className="nav-item">
                    <div className={"nav-link " + ((this.state.selectedTab === "vest") ? "active": "")}
                      onClick={() => this.selectTab("vest")}>Pending Vests</div>
                  </li>
                  <li className="nav-item">
                    <div className={"nav-link " + ((this.state.selectedTab === "grants") ? "active": "")}
                      onClick={() => this.selectTab("grants")}>Grants</div>
                  </li>
                </ul>
              </div>

              {this.state.selectedTab === "create" ? (
                <div id="new-grant">
                  <PendingRecipientList items={this.state.pendingRecipients} addItem={this.addItem} removeItem={this.removeItem}/>
                  <button id="submitGrants"
                          className="btn btn-primary"
                          disabled={!this.state.connected || this.state.pendingTx}
                          onClick={this.prepare_sender}>
                    {this.state.pendingTx ? (
                      <span className="spinner-border spinner-border-sm" role="status" aria-hidden="true"></span>
                    ) : ("Submit")}
                  </button>
                </div>
                ) : null}
              {this.state.selectedTab === "vest" ? (
                <div id="vest-list">
                  <VestList items={this.state.vests}/>
                </div>
                ) :null}
              {this.state.selectedTab === "grants" ? (
                <div id="vest-list">
                  <GrantList items={this.state.grants}/>
                </div>
                ) :null}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default App;
