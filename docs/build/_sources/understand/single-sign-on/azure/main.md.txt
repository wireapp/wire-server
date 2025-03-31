# How to set up SSO integration with Microsoft Azure

## Preprequisites

- <http://azure.microsoft.com> account, admin access to that account
- See also {ref}`sso-generic-setup`.

## Steps

### Azure setup

Go to <https://portal.azure.com/>, and click on 'Azure Active Directory'
in the menu to your left, then on 'Enterprise Applications':

```{image} 01.png
```

Click on 'New Application':

```{image} 02.png
```

Select 'Non-gallery application':

```{image} 03.png
```

Fill in user-facing app name, then click 'add':

```{image} 04.png
```

The app is now created.  If you get lost, you can always get back to
it by selecting its name from the enterprise applications list you've
already visited above.

Click on 'Configure single sign-on'.

```{image} 05.png
```

Select SAML:

```{image} 06.png
```

On the next page, you find a link to a configuration guide which you
can consult if you have any azure-specific questions.  Or you can go
straight to adding the two config parameters you need:

```{image} 07.png
```

Enter <https://prod-nginz-https.wire.com/sso/finalize-login> for both identity and reply url.  Save.

```{image} 08.png
```

Click on 'test later':

```{image} 09.png
```

Finally, you need to assign users to the newly created and configured application:

```{image} 11.png
```

```{image} 12.png
```

```{image} 13.png
```

```{image} 14.png
```

```{image} 15.png
```

And that's it!  You are now ready to set up your wire team for SAML SSO with the XML metadata file you downloaed above.

## Further reading

- technical concepts overview:
  : - <https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-saml-protocol-reference>
    - <https://docs.microsoft.com/en-us/azure/active-directory/develop/single-sign-on-saml-protocol>
- how to create an app:
  : - <https://docs.microsoft.com/en-us/azure/active-directory/develop/quickstart-register-app>
- how to configure SAML2.0 SSO:
  : - <https://docs.microsoft.com/en-us/azure/active-directory/manage-apps/what-is-single-sign-on#saml-sso>
    - <https://docs.microsoft.com/en-us/azure/active-directory/manage-apps/configure-single-sign-on-non-gallery-applications>
