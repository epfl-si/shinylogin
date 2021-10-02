# shinylogin

<!-- badges: start -->


<!-- badges: end -->

`shinylogin` is an extensible R package suited for implementing authentication in R Shiny apps. “Out of the box”, it provides a bare-bones implementation with passwords stored in `.htaccess` files, and an in-memory session cookie store.

## Installation

You can install the development version from github with the [remotes package](https://github.com/r-lib/remotes).

``` r
remotes::install_github("epfl-si/shinylogin")
```

## Run the example app

Code for an example app using [shinydashboard](https://rstudio.github.io/shinydashboard/) can be found in [inst/shiny-examples](inst/shiny-examples). You can launch it with the `runExample` function.

``` r
# login with user1 pass1 or user2 pass2
shinylogin::runExample()
```

## Usage

### Out-of-the-box password authentication

The package comes with simple, password-based authentication UI and server logic in which authentication (i.e. password check and user information retrieval), as well as (optional) session management (with cookies), are both user-overridable.

The provided `shinylogin::htpasswdAuth` authentication implementation uses a `.htaccess` file with the bcrypt password encryption scheme (that can be created or updated using `htpasswd -B`).

The provided `shinylogin::inMemoryCookieStore` cookie store implementation consists of an in-memory RSQLite database whose sole tuneable parameter is the cookie expiry time in days.

Neither of these provided functions / objects provide for any authorization-related metadata handling besides the user name; meaning that your app is in charge of implementing some kind of mapping from usernames to permissions, groups, roles or the like, if required.

Using `shinylogin` for password authentication requires calling the `shinylogin::passwordLogin` function, and inserting / calling the elements of the returned list inside your Shiny app as appropriate. [A complete example is provided](inst/shiny-examples/shinydashboard/app.R), which also demonstrates “manual” permissions management.

### Extending password authentication

The aforementioned `shinylogin::htpasswdAuth` and `shinylogin::inMemoryCookieStore` functions return “objects” (which are actually just plain lists of functions without class or generics). By implementing alternatives for these “objects”, and passing them into `shinylogin::passwordLogin`, one may

- authenticate against any password database e.g. an LDAP or Kerberos directory;
- piggy-back fetching and returning authorization user data (e.g. groups and roles) in the return value of the authorization function and cookie fetch method;
- enrich the behavior of cookie storage e.g. to make it persistent; or save additional user metadata (assuming this would be a wise thing to do, from a security standpoint, as opposed to retrieving them afresh upon every login); or implement a more sophisticated cookie renewal / deadline policy.

All of the above fully support async operations (by means of your code returning [promises](https://cran.r-project.org/web/packages/promises/index.html)).

A really simple [example](inst/shiny-examples/custom-auth/customauth.R) is provided, with a login button that logs you in as `joe` without any checks whatsoever.

### Other authentication techniques

Despite its capabilities for extensibility, the `shinylogin::passwordLogin` function is designed solely for the case where your app manipulates the passwords by itself — That is, logged-out users have access to a login / password form within the app, and the Shiny app “sees” the login and password as plain text, and is in charge of making the authentication and authorization decisions based on this sole input.

Many, if not most modern Web authentication scenarios do not fall within this purview. Two-factor authentication would require “more than one password field,” which the basic `shinylogin::passwordLogin` data flow doesn't account for. A very popular option is to achieve so-called *Web single sign-on* by delegating password handling to some other piece of infrastructure, such as a SAML or OpenID server; or alternatively, some kind of secure proxy sitting in front of the app and providing authentication metadata through a side-band channel such as cookies or request headers.

In such a case, you as the app author (or author of a `shinylogin.foo` extension module), can still build on top of the `shinylogin::core.*` functions. They provide you with a very basic state machine for the login workflow, and integrate with a one-size-fits-all (although optional) logout button UI.

## Credits

This package is a rewrite of [`shinyauthr`](https://cran.r-project.org/web/packages/shinyauthr/index.html), whose authors deserve full credit.

`shinyauthr` in turn originally borrowed some code from treysp's [shiny_password](https://github.com/treysp/shiny_password) template.

Thanks to [Michael Dewar](https://github.com/michael-dewar) for his contribution of cookie-based authentication. Some code was borrowed from calligross's [Shiny Cookie Based Authentication Example](https://gist.github.com/calligross/e779281b500eb93ee9e42e4d72448189) and from an earlier PR from [aqualogy](https://github.com/aqualogy/shinyauthr).


## Disclaimer

Neither the `shinyauthr` authors, nor the `shinylogin` authors can guarantee this authentication procedure to be foolproof. It is ultimately the shiny app developer's responsibility not to expose any sensitive content to the client without the necessary login criteria being met.
