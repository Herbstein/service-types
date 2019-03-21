declare class GlideRecord{ 
 addActiveQuery(): QueryCondition;
 addEncodedQuery(query: String): void;
 addJoinQuery(joinTable: String, primaryField: Object, joinTableField: Object): GlideQueryCondition;
 addNotNullQuery(fieldName: String): GlideQueryCondition;
 addNullQuery(fieldName: String): GlideQueryCondition;
 addQuery(query: String): GlideQueryCondition;
 addQuery(name: String, value: Object): GlideQueryCondition;
 addQuery(name: String, operator: String, value: Object): GlideQueryCondition;
 canCreate(): Boolean;
 canDelete(): Boolean;
 canRead(): Boolean;
 canWrite(): Boolean;
 chooseWindow(firstRow: Number, lastRow: Number, forceCount: Boolean): void;
 dateNumericValue(): Number;
 deleteMultiple(): void;
 deleteRecord(): Boolean;
 get(name: Object, value: Object): Boolean;
 getAttribute(fieldName: String): String;
 getClassDisplayValue(): String;
 getDisplayValue(): String;
 getED(): GlideElementDescriptor;
 getElement(columnName: String): GlideElement;
 getEncodedQuery(): String;
 getLabel(): String;
 getLastErrorMessage(): String;
 getLink(noStack: Boolean): String;
 getRecordClassName(): String;
 getRowCount(): Number;
 getTableName(): String;
 getUniqueValue(): String;
 getValue(name: String): String;
 hasNext(): Boolean;
 insert(): String;
 initialize(): void;
 isActionAborted(): Boolean;
 isNewRecord(): Boolean;
 isValid(): Boolean;
 isValidField(columnName: String): Boolean;
 isValidRecord(): Boolean;
 newRecord(): void;
 next(): Boolean;
 _next(): Boolean;
 operation(): String;
 orderBy(name: String): void;
 orderByDesc(name: String): void;
 query(field: Object, value: Object): void;
 _query(name: Object, value: Object): void;
 setAbortAction(b: Boolean): void;
 setDateNumericValue(milliseconds: Number): void;
 setLimit(maxNumRecords: Number): void;
 setNewGuidValue(guid: String): void;
 setValue(name: String, value: Object): void;
 setWorkflow(enable: Boolean): void;
 update(reason : String): String;
 updateMultiple(): void;
}

declare class GlideQueryCondition{ 
 addCondition(name : String, oper: String , value: Object): GlideQueryCondition;
 addOrCondition(name : String, oper: String , value: Object): GlideQueryCondition;
}

declare class GlideElementDescriptor{ 
 getAttachmentEncryptionType(): String;
 getEncryptionType(): String;
 getInternalType(): String;
 getLabel(): String;
 getLength(): Number;
 getName(): String;
 getPlural(): String;
 hasAttachmentsEncrypted(): Boolean;
 isAutoOrSysID(): Boolean;
 isChoiceTable(): Boolean;
 isEdgeEncrypted(): Boolean;
 isVirtual(): Boolean;
}

declare class GlideElement{ 
 canCreate(): Boolean;
 canRead(): Boolean;
 canWrite(): Boolean;
 changes(): Boolean;
 changesFrom(o: Object): Boolean;
 changesTo(o: Object): Boolean;
 getAttribute(attributeName: String): String;
 getBooleanAttribute(attributeName: String): Boolean;
 getChoices(dependent: String): Object[];
 getChoiceValue(): String;
 getDecryptedValue(): String;
 getDisplayValue(maxCharacters: Number): String;
 getED(): GlideElementDescriptor;
 getGlobalDisplayValue(): String;
 getHTMLValue(maxChars: Number): String;
 getJournalEntry(mostRecent: Number): String;
 getLabel(): String;
 getName(): String;
 getReferenceTable(): String;
 getRefRecord(): GlideRecord;
 getTableName(): String;
 nil(): Boolean;
 setDateNumericValue(milliseconds: Number): void;
 setDisplayValue(value: Object): void;
 setError(errorMessage: String): void;
 setPhoneNumber(phoneNumber: Object, strict: Boolean): Boolean;
 setValue(value: Object): void;
 toString(value: Object): String;
}