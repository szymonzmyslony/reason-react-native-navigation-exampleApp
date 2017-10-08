open ReactNative;

let render ::increment ::decrement ::goToMainScreen =>
  <View>
    <TouchableHighlight onPress=goToMainScreen>
      <Text value="GoToMainScreen" />
    </TouchableHighlight>
    <TouchableHighlight onPress=increment> <Text value="Increment" /> </TouchableHighlight>
    <TouchableHighlight onPress=decrement> <Text value="Decrement" /> </TouchableHighlight>
  </View>;
