# Model View Presenter Pattern (MVP) in Free Pascal / Lazarus
The Model View Presenter pattern seperates the view from the model and vice versa. Only the presenter is responsible to manage the interaction between the two. For more info see: https://en.wikipedia.org/wiki/Model–view–presenter.

## The example
My example demonstrates a little Lazarus project using the MVP pattern with just one form. It can be used to convert an ISO-Date string into an Unix timestamp and the other way round.

![Unix timestamp converter](https://dubst3pp4.github.io/projects/fpc_mvp_example/converter.png)

The example also shows how to achieve a "live binding" between model and view, so that when the model changes the view gets updated.

The main key of the MVP pattern is, that the view does not knows about the model or presenter and the model does not knows about the view or the presenter. The presenter is the only one, who knows **both**, the model *and* the view and interacts between its methods and properties (the presenter is the "clue" between model and interface). Therefore each of them, model and view, needs to provide an interface. This interface is the contract between the model or view and the presenter. With this interfaces it is easy to change the model with another implementation or the view with another one (for example for mobile devices), as long as the implementation follows the interface.



