/*
Recipient app structure
*/

class PendingRecipientList extends React.Component {
  render () {
    var items = this.props.items.map((item, index) => {
      return (
        <RecipientListItem key={index} item={item} index={index} removeItem={this.props.removeItem}/>
      );
    });

    return (
      <table className="table">
        <thead>
          <tr>
            <th style={{width: "5%"}} scope="col">#</th>
            <th style={{width: "40%"}} scope="col">Recipient</th>
            <th style={{width: "20%"}} scope="col">Vest Amount</th>
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
      <table className="table">
        <thead>
          <tr>
            <th style={{width: "5%"}} scope="col">#</th>
            <th style={{width: "10%"}} scope="col">Amount</th>
            <th style={{width: "20%"}} scope="col">Vest date (UTC)</th>
            <th style={{width: "30%"}} scope="col">Granter</th>
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
  constructor(props) {
    super(props);
  }

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
        <RecipientListItem key={index} item={item} index={index} removeItem={this.props.removeItem}/>
      );
    });

    return (
      <table className="table">
        <thead>
          <tr>
            <th style={{width: "5%"}} scope="col">#</th>
            <th style={{width: "40%"}} scope="col">Recipient</th>
            <th style={{width: "20%"}} scope="col">Vest Amount</th>
            <th style={{width: "25%"}} scope="col">Vest date (UTC)</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
        {items}
        </tbody>
      </table>
    );
  }
}

class RecipientListItem extends React.Component {
  constructor(props) {
    super(props);
    this.onClickClose = this.onClickClose.bind(this);
  }

  onClickClose() {
    var index = parseInt(this.props.index);
    this.props.removeItem(index);
  }

  render () {
    return(
      <tr>
        <th scope="row">{this.props.index+1}</th>
        <td>{this.props.item.value.addressValue}</td>
        <td>{this.props.item.value.amountValue}</td>
        <td>{this.props.item.value.deadlineValue}</td>
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
    this.onSubmit = this.onSubmit.bind(this);
    this.curTime = new Date().toISOString().split(":").slice(0, 2).join(":");
  }

  onSubmit(event) {
    event.preventDefault();
    var addressValue = this.refs.address.value;
    var amountValue = this.refs.amount.value;
    var deadlineValue =  this.refs.deadline.value;

    if(addressValue) {
      this.props.addItem({addressValue, amountValue, deadlineValue});
      this.refs.address.value = "";
      this.refs.amount.value = "";
      this.refs.deadline.value = "";
      this.refs.form.reset();
    }
  }
  render () {
    return (
      <tr>
        <th scope="row">{this.props.items.length+1}</th>
        <td>
          <input type="text" className="form-control" ref="address" placeholder="Cardano address"/>
        </td>
        <td>
          <input type="text" className="form-control" ref="amount" placeholder="Vest amount"/>
        </td>
        <td>
          <input type="datetime-local" className="form-control" min={this.curTime} ref="deadline"/>
        </td>
        <td>
          <form ref="form" onSubmit={this.onSubmit} className="form-inline">
            <button type="submit" className="btn btn-light">Add</button>
          </form>
        </td>
      </tr>
    );
  }
}

class CreateVestApp extends React.Component {
  constructor (props) {
    super(props);
    this.onChange = this.onChange.bind(this);
    this.addItem = this.addItem.bind(this);
    this.removeItem = this.removeItem.bind(this);
    this.submitRequest = this.submitRequest.bind(this);
    this.prepare_sender = this.prepare_sender.bind(this);
    this.connectWallet = this.connectWallet.bind(this);
    this.signTx = this.signTx.bind(this);
    this.sendTxAndWitnessBack = this.sendTxAndWitnessBack.bind(this);
    this.state = {
      pendingRecipients: [],
      vests: [],
      connected: false,
      selectedTab: "create",
      balance: 0,
    };

    ["nami", "eternl"].forEach((w) => {
      if (window.cardano[w] !== undefined) {
      window.cardano[w].isEnabled().then((enabled) => {
          if (enabled) {
            this.setState({connected: true});
            this.setState({wallet: window.cardano[w]});
            this.setState({icon: window.cardano[w].icon});
            window.cardano.getBalance().then((balance) => {
              var typedArray = new Uint8Array(balance.match(/[\da-f]{2}/gi).map(function (h) {
                return parseInt(h, 16)
              }))
              var b = CBOR.decode(typedArray.buffer) / 1000000;
              this.setState({balance: b});
            });
          }
        }
      ).catch((e) => {})
    }});
  }

  async componentDidMount() {
    await this.updateVestList();
  }

  onChange(e) {

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
    var usedAddresses = await window.cardano.getUsedAddresses();
    console.log(usedAddresses);
    var response = await fetch("get_vests?"+ new URLSearchParams({
        address: usedAddresses,
    }), {
      method: "GET",
      headers: {
        "Content-Type": "application/json"
      }
    });

    var j = await response.json();

    if (j.results.length > 0) {
      console.log(j.results);
      this.setState({vests: j.results});
    }
  }

  submitRequest(senders, change_address) {
    fetch('build_tx', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(
        {
          'senders': senders,
          'change_address': change_address,
          'recipients': this.state.pendingRecipients.map((item, index) => {
            return [item.value.addressValue, item.value.amountValue]
          })
        }
      )
    })
    .then(response => response.json())
    .then(this.signTx)
  }

  signTx(tx) {
    console.log(tx);
    window.cardano.signTx(tx['tx']).then((witness) => {
      this.sendTxAndWitnessBack(tx['tx'], witness)
    })
  }

  sendTxAndWitnessBack(tx, witness) {
    console.log(witness)
    fetch('submit_tx', {
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

  prepare_sender() {
    window.cardano.getUsedAddresses().then((senders) => {
        window.cardano.getChangeAddress().then((change_address) => {
            this.submitRequest(senders, change_address);
        })
    })
  }

  connectWallet(wallet) {
    if (!this.state.connected) {
      window.cardano[wallet].enable().then(
        () => {
          this.setState({connected: true});
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
              <a className="navbar-brand ms-3" href="#">Cardano Vesting</a>
              <div>
                {this.state.connected ? (
                  <div className="container">
                    <img style={{"width": "25px", "float": "left", "marginRight": "10px"}}
                               src={this.state.icon}/>
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
                    <ul className="dropdown-menu" style={{"float": "right"}}>
                      <li>
                        <div className="container dropdown-item wallet-icon" onClick={() => this.connectWallet("nami")}>
                          <img style={{"width": "25px", "float": "left", "marginRight": "10px"}}
                               src="static/assets/nami.svg"/>
                          <div style={{"float": "left"}}>Nami</div>
                        </div>
                      </li>
                      <li>
                        <div className="container dropdown-item wallet-icon" onClick={() => this.connectWallet("eternl")}>
                          <img style={{"width": "25px", "float": "left", "marginRight": "10px"}}
                               src="static/assets/eternl.svg"/>
                          <div style={{"float": "left"}}>Eternl</div>
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
                    <a className={"nav-link " + ((this.state.selectedTab === "create") ? "active": "")}
                      onClick={() => this.selectTab("create")}>Create</a>
                  </li>
                  <li className="nav-item">
                    <a className={"nav-link " + ((this.state.selectedTab === "vest") ? "active": "")}
                      onClick={() => this.selectTab("vest")}>Pending Vests</a>
                  </li>
                  <li className="nav-item">
                    <a className={"nav-link " + ((this.state.selectedTab === "grants") ? "active": "")}
                      onClick={() => this.selectTab("grants")}>Grants</a>
                  </li>
                </ul>
              </div>

              {this.state.selectedTab === "create" ? (
                <div id="new-grant">
                  <PendingRecipientList items={this.state.pendingRecipients} addItem={this.addItem} removeItem={this.removeItem}/>
                  <button id="submitGrants" className="btn btn-primary" disabled={!this.state.connected} onChange={this.onChange} onClick={this.prepare_sender}>Submit</button>
                </div>
                ) : null}
              {this.state.selectedTab === "vest" ? (
                <div id="vest-list">
                  <VestList items={this.state.vests}/>
                </div>
                ) :null}
              {/*<button className="btn btn-primary" disabled={this.state.connected} onChange={this.onChange} onClick={this.connectWallet}>Connect Nami wallet</button>*/}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

ReactDOM.render(<CreateVestApp/>, document.getElementById('app'));

