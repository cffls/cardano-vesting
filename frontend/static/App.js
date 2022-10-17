/*
Recipient app structure
*/
var recipientItems = [];

class RecipientList extends React.Component {
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
            <th style={{width: "25%"}} scope="col">Vest date</th>
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
        <td>{this.props.item.value.newAddressValue}</td>
        <td>{this.props.item.value.newAmountValue}</td>
        <td>{this.props.item.value.newDeadlineValue}</td>
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
  }

  onSubmit(event) {
    event.preventDefault();
    var newAddressValue = this.refs.address.value;
    var newAmountValue = this.refs.amount.value;
    var newDeadlineValue = this.refs.deadline.value;

    if(newAddressValue) {
      this.props.addItem({newAddressValue, newAmountValue, newDeadlineValue});
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
          <input type="date" className="form-control" ref="deadline" placeholder="Vest date"/>
        </td>
        <td>
          <form ref="form" onSubmit={this.onSubmit} className="form-inline">
            <button type="submit" className="btn btn-sm btn-primary">Add</button>
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
      recipientItems: recipientItems,
      connected: false
    };
    window.cardano.nami.isEnabled().then(
      (enabled) => {
          this.setState({connected: enabled})
      }
    )
  }
  onChange(e) {

  }
  addItem(recipientItem) {
    recipientItems.push({
      index: recipientItems.length+1,
      value: recipientItem,
    });
    this.setState({recipientItems: recipientItems});
  }
  removeItem (itemIndex) {
    recipientItems.splice(itemIndex, 1);
    this.setState({recipientItems: recipientItems});
  }

  submitRequest(senders, change_address) {
    console.log(senders);
    console.log(change_address);
    console.log(this.state.recipientItems);
    fetch('build_tx', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(
        {
          'senders': senders,
          'change_address': change_address,
          'recipients': this.state.recipientItems.map((item, index) => {
            return [item.value.newAddressValue, item.value.newAmountValue]
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

  connectWallet(event) {
    if (!this.state.connected) {
      window.cardano.nami.enable().then(
        () => {
          this.setState({connected: true})
        }
      );
    }
  }
  render() {
    return (
      <div className="main card">
        <div className="card-body">
            <RecipientList items={this.props.initItems} addItem={this.addItem} removeItem={this.removeItem}/>
            <button id="submitGrants" className="btn btn-primary" disabled={!this.state.connected} onChange={this.onChange} onClick={this.prepare_sender}>Submit Tx</button>
            {/*<button className="btn btn-primary" disabled={this.state.connected} onChange={this.onChange} onClick={this.connectWallet}>Connect Nami wallet</button>*/}
        </div>
      </div>
    );
  }
}

ReactDOM.render(<CreateVestApp initItems={recipientItems}/>, document.getElementById('app'));

