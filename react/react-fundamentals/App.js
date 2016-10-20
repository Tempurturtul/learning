import React from 'react';

class App extends React.Component {
  constructor() {
    super();
    this.state = {
      input: `\
const App = (props) => {
  var myStyle = {
    backgroundColor: '#000',
    height: 10 // px added by React.
  };

  return (
    <div style={myStyle}>
      <a href="#"
        notrendered="x"
        data-rendered="y"
        onClick={update}>
        {/* this is a comment */}
        this is the text
        {i>1 ? 'More than one' : 'one'}
        {i>1 && 'More than one'}
      </a>
    </div>
  );
};`,
      output: '',
      err: ''
    };
    this.update = this.update.bind(this);
  }

  update(e) {
    let code = e.target.value;
    try {
      this.setState({
        output: babel.transform(code, {
          stage: 0,
          loose: 'all'
        }).code,
        err: ''
      });
    }
    catch(err) {
      this.setState({err: err.message});
    }
  }

  render() {
    return (
      <div>
        <header>{this.state.err}</header>
        <div className="container">
          <textarea
            onChange={this.update}
            defaultValue={this.state.input}>
          </textarea>
          <pre>
            {this.state.output}
          </pre>
        </div>
      </div>
    );
  }
}

export default App;
