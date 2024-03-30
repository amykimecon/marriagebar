from typing import Optional, Sequence, List
from google.api_core.client_options import ClientOptions
from google.cloud import documentai  # type: ignore
from google.cloud.documentai_toolbox import document
import pandas as pd
import os

os.chdir("./Dropbox (Princeton)/marriagebar")

# TODO(developer): Uncomment these variables before running the sample.
project_id = "hallowed-name-293714"
location = "us" # Format is "us" or "eu"
processor_id = "22b1d123eb00c51b" # Create processor before running sample
#file_path = "raw_scans/NC/nc_testtable_new.png"
file_path = "raw_scans/NC/NC_1931_32_stats.pdf"
#mime_type = "image/png" # Refer to https://cloud.google.com/document-ai/docs/file-types for supported file types
mime_type = "application/pdf"
# field_mask = "text,entities,pages.pageNumber"  # Optional. The fields to return in the Document object.
processor_version_id = "pretrained-form-parser-v2.0-2022-11-10" # Optional. Processor version to use


# def process_document_sample(
#     project_id: str,
#     location: str,
#     processor_id: str,
#     file_path: str,
#     mime_type: str,
#     field_mask: Optional[str] = None,
#     processor_version_id: Optional[str] = None,
# ) -> None:
#     # You must set the `api_endpoint` if you use a location other than "us".
#     opts = ClientOptions(api_endpoint=f"{location}-documentai.googleapis.com")

#     client = documentai.DocumentProcessorServiceClient(client_options=opts)

#     if processor_version_id:
#         # The full resource name of the processor version, e.g.:
#         # `projects/{project_id}/locations/{location}/processors/{processor_id}/processorVersions/{processor_version_id}`
#         name = client.processor_version_path(
#             project_id, location, processor_id, processor_version_id
#         )
#     else:
#         # The full resource name of the processor, e.g.:
#         # `projects/{project_id}/locations/{location}/processors/{processor_id}`
#         name = client.processor_path(project_id, location, processor_id)

#     # Read the file into memory
#     with open(file_path, "rb") as image:
#         image_content = image.read()

#     # Load binary data
#     raw_document = documentai.RawDocument(content=image_content, mime_type=mime_type)

#     # Configure the process request
#     request = documentai.ProcessRequest(
#         name=name, raw_document=raw_document, field_mask=field_mask
#     )

#     result = client.process_document(request=request)

#     # For a full list of `Document` object attributes, reference this page:
#     # https://cloud.google.com/document-ai/docs/reference/rest/v1/Document
#     document = result.document

#     # Read the text recognition output from the processor
#     print("The document contains the following text:")
#     print(document.text)

def process_document_form_sample(
    project_id: str,
    location: str,
    processor_id: str,
    processor_version_id: str,
    file_path: str,
    mime_type: str,
) -> documentai.Document:
    # Online processing request to Document AI
    document = process_document(
        project_id, location, processor_id, processor_version_id, file_path, mime_type
    )

    # Read the table and form fields output from the processor
    # The form processor also contains OCR data. For more information
    # on how to parse OCR data please see the OCR sample.

    text = document.text
    print(f"Full document text: {repr(text)}\n")
    print(f"There are {len(document.pages)} page(s) in this document.")

    # Read the form fields and tables output from the processor
    for page in document.pages:
        print(f"\n\n**** Page {page.page_number} ****")

        print(f"\nFound {len(page.tables)} table(s):")
        for table in page.tables:
            num_columns = len(table.header_rows[0].cells)
            num_rows = len(table.body_rows)
            print(f"Table with {num_columns} columns and {num_rows} rows:")

            # Print header rows
            print("Columns:")
            print_table_rows(table.header_rows, text)
            # Print body rows
            print("Table body data:")
            print_table_rows(table.body_rows, text)

        # print(f"\nFound {len(page.form_fields)} form field(s):")
        # for field in page.form_fields:
        #     name = layout_to_text(field.field_name, text)
        #     value = layout_to_text(field.field_value, text)
        #     print(f"    * {repr(name.strip())}: {repr(value.strip())}")

    # Supported in version `pretrained-form-parser-v2.0-2022-11-10` and later.
    # For more information: https://cloud.google.com/document-ai/docs/form-parser
    if document.entities:
        print(f"Found {len(document.entities)} generic entities:")
        for entity in document.entities:
            print_entity(entity)
            # Print Nested Entities
            for prop in entity.properties:
                print_entity(prop)

    # wrapped_document = document.Document

    # for page in wrapped_document.pages:
    #     for table_index, table in enumerate(page.tables):
    #         # Convert table to Pandas Dataframe
    #         # Refer to https://pandas.pydata.org/docs/reference/frame.html for all supported methods
    #         df = table.to_dataframe()
    #         print(df)

    #         output_filename = f"{nctable-out}-{page.page_number}-{table_index}"

    #         # Write Dataframe to CSV file
    #         df.to_csv(f"{output_filename}.csv", index=False)

    return document


def print_table_rows(
    table_rows: Sequence[documentai.Document.Page.Table.TableRow], text: str
) -> None:
    for table_row in table_rows:
        row_text = ""
        for cell in table_row.cells:
            cell_text = layout_to_text(cell.layout, text)
            row_text += f"{repr(cell_text.strip())} | "
        print(row_text)


def print_entity(entity: documentai.Document.Entity) -> None:
    # Fields detected. For a full list of fields for each processor see
    # the processor documentation:
    # https://cloud.google.com/document-ai/docs/processors-list
    key = entity.type_

    # Some other value formats in addition to text are availible
    # e.g. dates: `entity.normalized_value.date_value.year`
    text_value = entity.text_anchor.content
    confidence = entity.confidence
    normalized_value = entity.normalized_value.text
    print(f"    * {repr(key)}: {repr(text_value)}({confidence:.1%} confident)")

    if normalized_value:
        print(f"    * Normalized Value: {repr(normalized_value)}")


def process_document(
    project_id: str,
    location: str,
    processor_id: str,
    processor_version_id: str,
    file_path: str,
    mime_type: str,
    process_options: Optional[documentai.ProcessOptions] = None,
) -> documentai.Document:
    # You must set the `api_endpoint` if you use a location other than "us".
    client = documentai.DocumentProcessorServiceClient(
        client_options=ClientOptions(
            api_endpoint=f"{location}-documentai.googleapis.com"
        )
    )

    # The full resource name of the processor version, e.g.:
    # `projects/{project_id}/locations/{location}/processors/{processor_id}/processorVersions/{processor_version_id}`
    # You must create a processor before running this sample.
    name = client.processor_version_path(
        project_id, location, processor_id, processor_version_id
    )

    # Read the file into memory
    with open(file_path, "rb") as image:
        image_content = image.read()

    # Configure the process request
    request = documentai.ProcessRequest(
        name=name,
        raw_document=documentai.RawDocument(content=image_content, mime_type=mime_type),
        # Only supported for Document OCR processor
        process_options=process_options,
    )

    result = client.process_document(request=request)

    # For a full list of `Document` object attributes, reference this page:
    # https://cloud.google.com/document-ai/docs/reference/rest/v1/Document
    return result.document

def get_table_data(
    rows: Sequence[documentai.Document.Page.Table.TableRow], text: str
) -> List[List[str]]:
    """
    Get Text data from table rows
    """
    all_values: List[List[str]] = []
    for row in rows:
        current_row_values: List[str] = []
        for cell in row.cells:
            current_row_values.append(
                text_anchor_to_text(cell.layout.text_anchor, text)
            )
        all_values.append(current_row_values)
    return all_values

def text_anchor_to_text(text_anchor: documentai.Document.TextAnchor, text: str) -> str:
    """
    Document AI identifies table data by their offsets in the entirity of the
    document's text. This function converts offsets to a string.
    """
    response = ""
    # If a text segment spans several lines, it will
    # be stored in different text segments.
    for segment in text_anchor.text_segments:
        start_index = int(segment.start_index)
        end_index = int(segment.end_index)
        response += text[start_index:end_index]
    return response.strip().replace("\n", " ")


def layout_to_text(layout: documentai.Document.Page.Layout, text: str) -> str:
    """
    Document AI identifies text in different parts of the document by their
    offsets in the entirety of the document"s text. This function converts
    offsets to a string.
    """
    # If a text segment spans several lines, it will
    # be stored in different text segments.
    return "".join(
        text[int(segment.start_index) : int(segment.end_index)]
        for segment in layout.text_anchor.text_segments
    )

def table_sample(document_path: str, output_file_prefix: str) -> None:
    wrapped_document = document.Document.from_document_path(document_path=document_path)

    print("Tables in Document")
    for page in wrapped_document.pages:
        for table_index, table in enumerate(page.tables):
            # Convert table to Pandas Dataframe
            # Refer to https://pandas.pydata.org/docs/reference/frame.html for all supported methods
            df = table.to_dataframe()
            print(df)

            output_filename = f"{output_file_prefix}-{page.page_number}-{table_index}"

            # Write Dataframe to CSV file
            df.to_csv(f"{output_filename}.csv", index=False)

            # Write Dataframe to HTML file
            df.to_html(f"{output_filename}.html", index=False)

            # Write Dataframe to Markdown file
            df.to_markdown(f"{output_filename}.md", index=False)

testdoc = process_document_form_sample(project_id = project_id, location = location, processor_id = processor_id, processor_version_id = processor_version_id, file_path = file_path, mime_type = mime_type)
header_row_values: List[List[str]] = []
body_row_values: List[List[str]] = []

for page in testdoc.pages:
    for table in page.tables:
        header_row_values = get_table_data(table.header_rows, testdoc.text)
        body_row_values = get_table_data(table.body_rows, testdoc.text)

# Create a Pandas Dataframe to print the values in tabular format.
df = pd.DataFrame(
    data=body_row_values,
    columns=pd.MultiIndex.from_arrays(header_row_values),
)

print(df)

df.to_csv("testnc2.csv", index=False)