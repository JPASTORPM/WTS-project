# Wastewater Treatment System (WTS) project

This WTS-project in R aimed to evaluate the spatial dynamics of some of the physicochemical parameters in an artificial wetland system of sub-superficial flow of _Pennisetum alopecuroides_ (Pennisetum) and a control (non-plants). The purpose is to provide a simple example of an analysis of the spatial dynamics through the use of the R programming language. Each of the cells (Pennisetum and control) had 12 piezometers, organized in three columns and four rows with a separation distance of 3.25m and 4.35m, respectively. It was measured in each of the piezometers the _oxidation-reduction potential (ORP)_, _dissolved oxygen (OD)_, _conductivity_, _pH_ and _water temperature_ (_n_ = 167). The monitoring of the spatial dynamics of these parameters and other variables could show us if there is any obstruction of the flow and/or possible reduction of the removal by the plants. An open-source repository of R was provided.

### Exemples

![Fig. Spatial dynamics pf ORP](https://github.com/JPASTORPM/WTS-project/blob/master/Results/Fig.%20ORP.png)

> _Spatial distribution of ORP levels within the systems (Control and Pennisetum) based on bilinear interpolations between the piezometers. Colour gradient and contour lines indicate parameter intensity from low (blue) to high (red), and white crosses indicate the position of the piezometers; Boxplots show the comparison between rows and columns of the position of the piezometers, where, box marks Q1 and Q3, the black line is median (Q2), lines shown maximum and minimum values, and circles are values outliers with three times greater than the mean; Subplot with colour gradient indicate flow direction in y axis_


![Fig. Spatial dynamics pf ORP](https://github.com/JPASTORPM/WTS-project/blob/master/Results/Fig.%20Conductivity.png)

> _Spatial distribution of water conductivity levels within the systems (Control and Pennisetum) based on bilinear interpolations between the piezometers. See legend explanation in the previous figure._


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

What things you need to install the software and how to install them

```
Give examples
```

### Installing

A step by step series of examples that tell you how to get a development env running

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Junior Pastor Pérez Molina** - *Initial work* - [JPASTORPM](https://github.com/JPASTORPM)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc
