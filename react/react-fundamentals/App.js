import React from 'react';
import ReactDOM from 'react-dom';

class App extends React.Component {
  render() {
    let txt = this.props.txt;
    return <h1>{txt}</h1>;
  }
}

// Identifies the types of properties expected on the component.
App.propTypes = {
  txt: React.PropTypes.string,
  cat: React.PropTypes.number.isRequired
}

// Default properties.
App.defaultProps = {
  txt: 'this is the default txt'
};

ReactDOM.render(
  <App cat={5} txt="this is the prop's value" />,
  document.getElementById('app')
);

export default App;
