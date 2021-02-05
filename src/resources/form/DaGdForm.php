<?php

/**
 * A form with (optional, enabled by default) CSRF protection. It assumes quite
 * a bit, but is also flexible enough to be subclassed when necessary to curtail
 * some of those assumptions.
 *
 * Controllers will typically create a DaGdForm and add fields by using
 * `addField()`.
 *
 * DaGdForm will use and manipulate the DaGdRequest directly (storing CSRF in
 * its DaGdSession, for example, and using its getRequest() to read $_REQUEST.
 * Thus, controllers creating DaGdForm instances must call `setRequest()` and
 * set it to the current request. Typically in a controller, this will look
 * like:
 *   $form->setRequest($this->getRequest());
 *
 * On POST, they will call `isValid()`. If `true`, all is good, otherwise you
 * can loop/render `getErrors()` for global errors (such as bad CSRF token).
 *
 * Also, when `isValid()` is false, fields within the form will get errors added
 * to them. The next time the form is rendered, the errors will be rendered next
 * to the fields as well.
 *
 * Creating various kinds of form fields entails subclassing DaGdFormField. Such
 * subclasses can implement isValid() to decide what constitutes valid data.
 */
class DaGdForm implements DaGdToTagInterface {
  private $action;
  private $method = 'POST';
  private $classes = array();
  private $wants_csrf = true;
  private $csrf_field;
  private $fields = array();
  private $request;
  private $errors;

  public function setAction($action) {
    $this->action = $action;
    return $this;
  }

  public function getAction() {
    return $this->action;
  }

  public function setMethod($method) {
    $this->method = $method;
    return $this;
  }

  public function getMethod() {
    return $this->method;
  }

  public function setClasses($classes) {
    $this->classes = $classes;
    return $this;
  }

  public function addClass($class) {
    $this->classes[] = $class;
    return $this;
  }

  public function getClasses() {
    return $this->classes;
  }

  public function setWantsCsrf($wants_csrf) {
    $this->wants_csrf = $wants_csrf;
    return $this;
  }

  public function getWantsCsrf() {
    return $this->wants_csrf;
  }

  // Only used internally
  private function setRandomCsrf() {
    $length = DaGdConfig::get('session.form_csrf_length');
    $csrf = randstr($length);
    $this->getRequest()->getSession()->set('csrf', $csrf);
    $this->csrf_field = id(new DaGdFormHiddenField('csrf'))
      ->setValue($csrf);
    return $this;
  }

  public function getCsrfField() {
    return $this->csrf_field;
  }

  public function setFields($fields) {
    $this->fields = $fields;
    return $this;
  }

  public function addField(DaGdFormField $field, $throw_on_duplicate = true) {
    if ($throw_on_duplicate) {
      if (array_key_exists($field->getName(), $this->fields)) {
        throw new Exception('Field names must be unique: '.$field->getName());
      }
    }
    $this->fields[$field->getName()] = $field;
    return $this;
  }

  public function getField($name) {
    return idx($this->fields, $name);
  }

  public function getFields() {
    return $this->fields;
  }

  public function setRequest($request) {
    $this->request = $request;
    return $this;
  }

  public function getRequest() {
    return $this->request;
  }

  public function setErrors($errors) {
    $this->errors = $errors;
    return $this;
  }

  public function addError($error) {
    $this->errors[] = $error;
    return $this;
  }

  public function getErrors() {
    return $this->errors;
  }

  public function isValid() {
    $vars = $this->getRequest()->getRequest();
    $valid = true;

    if ($this->getWantsCsrf()) {
      $provided = idx($vars, 'csrf');
      $wanted = $this->getRequest()->getSession()->get('csrf');
      if (!$provided || !$wanted || $provided !== $wanted) {
        $valid = false;
        $this->addError('Invalid CSRF token');
      }
    }

    foreach ($this->getFields() as $field) {
      $data = idx($vars, $field->getName(), '');
      $field->setValue($data);
      if (!$field->isValid()) {
        $valid = false;
      }
    }
    return $valid;
  }

  public function toTag() {
    if ($this->getWantsCsrf()) {
      $this->setRandomCsrf();
      $this->addField($this->getCsrfField());
    }

    $field_tags = array();
    foreach ($this->getFields() as $name => $field) {
      $field_tags[] = $field;
    }

    $form = tag(
      'form',
      $field_tags,
      array(
        'action' => $this->getAction(),
        'method' => $this->getMethod(),
        'class' => implode(' ', $this->getClasses()),
      )
    );
    return $form;
  }
}
